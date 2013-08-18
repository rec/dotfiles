
################################################################################
#
#   Driving KeyKit from a local TCP/IP connection
#
#       this makes it possible to have a KeyKit console within Emacs
#       ... see Emacs keykit-mode documentation for details:
#       http://www.zogotounga.net/GM/GMemacs.html
#
#       this file is a hack of lib/tcpip.k from the GeoMaestro distribution
#       you should remove it from your Keypath if you install Geomaestro
#
################################################################################


#name	launch_daemon_tcpip
#usage	launch_daemon_tcpip()
#desc	starts a TCP/IP connection intended to interface KeyKit with Emacs
#desc	this must be followed, on Emacs side, by an evaluation of (kk-tcpip)
#desc	which is provided by keykit-mode (in kk_utils.el)
#see	kill_daemon_tcpip
#topics	emacs, system

function launch_daemon_tcpip()
{
    if (!defined(TCP_IP_localport))
        TCP_IP_localport = 5861  
    if (!defined(TCP_IP_daemon_is_a_daemon))
        TCP_IP_daemon_is_a_daemon = 0  
    if (!defined(TCP_IP_prompt))
        TCP_IP_prompt = "key> "    

    TCP_IP_daemon_do_not_restart = 0
    
    if (!defined(TCP_IP_out) || TCP_IP_out == 0)
        task daemon_tcpip()
    else
        print("... already listening to "+string(TCP_IP_localport)+"@127.0.0.1 !!")
}


#name	kill_daemon_tcpip
#usage	kill_daemon_tcpip()
#desc	close the TCP/IP connection with Emacs and do not try to restore it
#see	launch_daemon_tcpip
#topics	emacs, system

function kill_daemon_tcpip()
{
    TCP_IP_daemon_do_not_restart = 1  # neutralise daemon_tcpip_restart()
    kill(TCP_IP_get_ID)    
    print("<TCP/IP: aborting>")
}

function daemon_tcpip_restart()
{
    if (TCP_IP_daemon_do_not_restart)
        return()
    if (TCP_IP_daemon_is_a_daemon ||
            OuiOui(getastr("Restart daemon_tcpip() ? > ","y")))
        task daemon_tcpip()
}

function daemon_tcpip() {
    onexit(daemon_tcpip_restart)

    port = TCP_IP_localport
    a = open(string(port)+"@127.0.0.1","rf","tcpip_listen")
    if ( a == "" )
    {
        TCP_IP_daemon_do_not_restart = 1
        error("Can't open tcpip_listen")
    }

    else
    {
        print("<TCP/IP: listening to "+string(port)+"@127.0.0.1 : "+string(a)+">")
        tcpip_evalloop(tcpip_get(a))
    }
    
    close(a)
    print("<TCP/IP: deamon died>")
}

function tcpip_get(f)
{
    TCP_IP_abort = 0    
    TCP_IP_input = ""
    TCP_IP_get_ID = task TCP_IP_get(f)
    wait(TCP_IP_get_ID)

    if (TCP_IP_input == "")  # interruption par kill_daemon_tcpip()
        return(Eof)

    return(TCP_IP_input)
}

function TCP_IP_get(f){
    TCP_IP_input = get(f)
}

function tcpip_evalloop(x)  # boucle d'évaluation de la console sous Emacs
{
    if (x == Eof)
        return()
    
    print("<TCP/IP: got connection>")
    
    TCP_IP_ELispFiFo = open()    
        
    TCP_IP_wtid = 0
    TCP_IP_out = x["w"]
    TCP_IP_in = x["r"]
    fifoctl(x["r"],"type","l")
    
    put(TCP_IP_out,tcpip_header())
    
    put(TCP_IP_out,TCP_IP_prompt)
    while ((d = tcpip_get(x["r"])) != Eof ) 
    {
        TCP_IP_evaluating = 1
        TCP_IP_blocking = 0

        d = RIS(d,"\n","","\r","") 
        
        # Hist management (copied and adapted from interploop()
        if (d == "!")
        {
            if (Histindex > 0) 
                for(hi = 0; hi < Histindex; hi++)
                    printf("%d: %s\n",hi,Hist[hi])
            else 
                printf("no history yet...\n")
            d = ""
        }           
        else if (d == "!!!")
        {
            Histindex = 0
            d = ""
        }
        else if (d == "!!") 
        {
            if (Histindex > 0)
                d = Hist[Histindex-1]
            else
            {
                printf("no history yet...\n")
                d = ""
            }
        }    
        else if (substr(d,1,1)=="!" && typeof(which = integer(split(d,"!")[0]))=="integer")
        {
            if (which < 0)
                which = Histindex + which
            if (which >= 0 && which <= Histindex) 
            {                
                d = Hist[which]
                printf("%s\n",">>> "+d)
            }
            else
                d = ""
        }
        #end of Hist management

        if (d != "")
        {
            if (substr(d,1,1) == ":")
                d = "print("+substr(d,2)+")"
            
            if (substr(d,1,3) == ">>>")
            {
                print(d)
                d = substr(d,4)
            }       
            
            if (substr(d,1,3) == "<->")
            {
                send_evalue_back = 1
                d = substr(d,4)
            }
            else
                send_evalue_back = 0
            
            if (substr(d,1,7) == "<ELisp>")       
            {
                # flush previous blocked data (hmm... we need an error message here)
                while (fifosize(TCP_IP_ELispFiFo))
                    get(TCP_IP_ELispFiFo)
                
                put(TCP_IP_ELispFiFo, substr(d,8))
            }
            else
            {         # à revoir (multiples connections TCP/IP ?):
                if (defined(TCP_IP_evalid)) # kill conflicting task (error here, also)
                    kill(TCP_IP_evalid)       
                TCP_IP_evalid = task tcpip_protected_eval(d, send_evalue_back)     
                # wait(TCP_IP_evalid)
            } 
        }
        else
            put(TCP_IP_out,TCP_IP_prompt)
    
        TCP_IP_evaluating = 0
        TCP_IP_blocking = 1    
    }
    
    print("<TCP/IP: closing fifos>")
    TCP_IP_out = 0
    TCP_IP_in = 0
    close(x["r"])
    close(x["w"])   
    
    close(TCP_IP_ELispFiFo)     

    kill(TCP_IP_wtid)  
}

function tcpip_header()
{
    return(  "\n"
           + "--------------------------\n"
           + " KeyKit v"+Version+"\n"
           + "--------------------------\n"
           + "<Connected>\n\n" 
           )
}


function tcpip_protected_eval(d, send_evalue_back)
{
    TCP_IP_erreur = ((TCP_IP_evalue = Eval(d)) == EvalErrorStatement)
    
    if (TCP_IP_erreur && TCP_IP_out)
        put(TCP_IP_out,"** evaluation error **\n"+TCP_IP_prompt)
    
    else if (!TCP_IP_erreur)
    {
        if (typeof(TCP_IP_evalue) == "phrase")
            realtime(TCP_IP_evalue)
        
        if (send_evalue_back)
            put(TCP_IP_out,"\n<->"+string(TCP_IP_evalue)+"</>\n")
        
        put(TCP_IP_out,TCP_IP_prompt)
    }
    else if (send_evalue_back)
        put(TCP_IP_out,"\n<->** evaluation error **</>\n")                   
    
    if (RecordEmacsCommandsInHist)
        Hist[Histindex++] = d
}


################################################################################
#
#   print() and printf()
#
################################################################################


# this overrides the print() code in lib/util1.k
#  so that its arguments are also send to TCP_IP_out if it exists and is open
function TCPIP_print(...)      
{
    sep = ""
    if (!defined(TCP_IP_out))
        TCP_IP_out = 0
    
    if ( Graphics && defined(Console) ) {
        lock(Console)
        for ( n=0; n<nargs(); n++ ) {
            Console.printf("%s%s",sep,string(argv(n)))
            if (TCP_IP_out != 0)
                put(TCP_IP_out,sep+string(argv(n)))
            sep = " "
        }
        Console.printf("\n")
        if (TCP_IP_out != 0)
            put(TCP_IP_out,"\n")
        unlock(Console)
    }
    else {
        for ( n=0; n<nargs(); n++ ) {
            printf("%s%s",sep,string(argv(n)))
        }
        printf("\n")
    }
}
print = TCPIP_print


# this overrides printf (a bit more tricky):

if (!defined(GENUINE_printf)) 
{
    GENUINE_printf = printf      # must be set only once, at startup
    eval("function printf(...){GENUINE_printf(...);if(TCP_IP_out != 0)put(TCP_IP_out,sprintf(...))}")
}




################################################################################
#
#   Evaluating Emacs Lisp from KeyKit
#
################################################################################



#name	ELisp
#usage	ELisp(str)
#desc	have its argument (a string) evaluated as Emacs Lisp code
#desc	this require a TCP/IP connection between KeyKit and Emacs 
#see	launch_daemon_tcpip
#topics	emacs, system

function ELisp(str)
{
    if (!defined(TCP_IP_elisp_error))
        TCP_IP_elisp_error = "<evaluation error>"
    
    if (TCP_IP_out)
    {
        put(TCP_IP_out,"\n<ELisp>"+str+"</ELisp>\n") 
        if (TCP_IP_evaluating)
            return(substr(get(TCP_IP_in),8))
        else
            return(get(TCP_IP_ELispFiFo))
    }
    
    return("")
}

# handy:
Elisp = ELisp




################################################################################
#
#   utilies
#
################################################################################

#name	RIS
#usage	RIS(str, str1, rep1 {, str2 {, rep2 {...]]])
#desc	replaces all occurences of strn in str with repn
#desc	(all arguments are strings) 
#desc	   
#desc	ex:
#desc	   RIS("abcdefgh","a","AA","e","aa")  
#desc	returns
#desc	   "AAbcdaafg"
#topics	string

function RIS(str, ...)  
{
    for (i = 1; i<= (nargs()-1)/2; i++)
        str = ReplaceInStr(str, argv(2*(i-1)+1), argv(2*i))
          
    return (str)
}

function ReplaceInStr(sent, cha1, cha2)
{
    if (1 in StrCut(cha2,cha1))           
    {
        sent = ReplaceInStr(sent, cha1, "£§µ")
        sent = ReplaceInStr(sent, "£§µ",cha2)
        return (sent)
    }
    
    lc1 = sizeof(cha1)
    fin = 0
    while (fin == 0)
    {
        coupe = []
        coupe = StrCut(sent,cha1)

        if (1 in coupe) 
            sent = (coupe[0]+cha2+substr(coupe[1],lc1+1,sizeof(sent)-lc1))
        else fin=1
    }
    
    return (sent)
}



#name	StrCut
#usage	StrCut(str, arg2 {, keepf})
#desc	splits str at the character number arg2 (if an integer) 
#desc	or at the beginning of substring arg2 (if a string) 
#desc	and returns the two parts in an array
#desc	
#desc	ex:
#desc	   StrCut("abcdefgh",4)  is [0="abc",1="defgh"]
#desc	   StrCut("abcdefgh","c")  is [0="ab",1="cdefgh"]
#desc	   StrCut("abcdefgh","def")  is [0="abc",1="defgh"]
#desc	   StrCut("abcdefgh","df")  is [0="abcdefgh"]
#desc	   StrCut("abcdefgh","a")  is [0="",1="abcdefgh"]
#desc	
#desc	the field 0 is always present, but can be an empty string
#desc	the field 1 is only present if arg2 is in str
#desc	
#desc	if keepf is 0, the "edge" character is suppressed:
#desc	   StrCut("abcdefgh","def",0)  is [0="abc",1="efgh"]
#desc	   StrCut("abcdefgh","h",0)  is [0="abcdefg", 1=""]
#topics	string

function StrCut(str, arg2, keepf)    # arg2 integer ou string 
{
    if (nargs() < 3)
        keepf = 1

    len = sizeof(str)   
    if (typeof(arg2) == "string")
    {
        l2 = sizeof(arg2)
        
        nlast = 0
        if (substr(str,Maximum(len-l2+1,1),l2) == arg2)
            nlast = len-l2+1

        if (substr(str,1,l2) == arg2)   
            n = 1
        else
        {
            un = substr(arg2,1,1)
            if (substr(str,1,1) == un) n = 1 else n = 0
            ms = split(str,un)

            if (!(1 in ms) && nlast==0)
                return ([0= str])
            if (!(1 in ms))
                n = nlast
            else
            {
                oui = 0
                n = n+sizeof(ms[0])+1              
                while (substr(str,n,1)==un)
                {
                    if (substr(str,n,l2) != arg2)
                        n+=1
                    else
                        return(StrCut(str,n,keepf))
                }
                n -= 1

                for (i=1; (i<sizeof(ms)) && (oui==0); i++) 
                {
                    if (substr(str,n,l2) == arg2) 
                    {
                        oui = 1 
                        break
                    }
                    else
                    {                        
                        n = n+sizeof(ms[i])+1               
                        while (substr(str,n,1)==un)
                        {
                            if (substr(str,n,l2) != arg2)
                                n+=1
                            else
                                return(StrCut(str,n,keepf))
                        }
                        n -= 1
                    }

                }
                
                if (oui==0 && nlast==0) 
                    return([0= str])
                
                if (oui==0)
                    n = nlast
            }
        }
    }
    else n = arg2       
    
    if (n>1 && n<=len)
    { 
        return([
                0= substr(str,1,n-1), 
                1 = (keepf ? substr(str,n,len-n+1) : substr(str,n+1,len-n))] )
    }
    else if (n==1)    
        return( [0= "", 1= (keepf ? str : substr(str,2))] )
}


#name	Maximum
#usage	Maximum(a,b)
#desc	returns a if a>b, else returns b
#topics	math, programming

function Maximum(a,b)
{ return(b > a ? b : a)}




#name	Eval
#usage	Eval(codestring {, str1, val1 {, str2, val2 {,..}}})
#desc	A enhanced version of eval() supporting multithreading:
#desc	eval("T_a_b_f__[n=uniqnum()] = "+codestring) is performed and T_a_b_f__[n] is
#desc	returned. 
#desc	 
#desc	The T_a_b_f__ array is then the only global variable used by Eval,
#desc	and the uniqnum() calls make it possible to call several Eval at the same time
#desc	without conflicts.
#desc	 
#desc	Examples:
#desc	      coms = "1+1"
#desc	      a = Eval(coms)
#desc	      eval("T_ ="+coms)
#desc	      b = T_
#desc	... we get a==b, but no use of T_ was necessary to calculate a
#desc	 
#desc	Use FlushEvalSpace() to empty the T_a_b_f__ array (this is automatically performed
#desc	by GeoMaestro GUIs as often as possible)
#desc	 
#desc	Other arguments are couples strn/valn, for which each occurrence of strn (a string) in
#desc	codestring will be replaced by valn (any value) before evaluation
#desc	
#desc	Example:
#desc	      a = Eval("cos(Q)+P", "P", 1, "Q", 3.14159)
#desc	
#desc	a is 0; note that no specific global variable was necessary: "P" and "Q" are
#desc	only strings, not variables.
#see	MEval, FlushEvalSpace
#topics	programming

function Eval(evalstr, ...)             
{
    if (typeof(evalstr) != "string")
        return(evalstr)

    if (!defined(EvalErrorStatement))
        EvalErrorStatement = "Eval: evaluation error"

    if (nargs() > 1)
        for(i=1; i<nargs(); i+=2)
            evalstr = ReplaceInStr(evalstr, argv(i), "("+string(argv(i+1))+")")

    if (!defined(T_a_b_f__))
        T_a_b_f__=[]

    if (eval("T_a_b_f__["+string(n = uniqnum())+"]="+evalstr)) 
    {
        delete T_a_b_f__[n]
        return(EvalErrorStatement)
    }
    else 
    {
        result = T_a_b_f__[n]
        delete T_a_b_f__[n]
        if (defined(result))
            return(result)
        else
            return()
    }
}

