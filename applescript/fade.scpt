set fadeDuration to 5
set fadeStepDuration to 0.017 -- Apple says this should be > 0.01667 seconds

tell application "Music"
    set isPlaying to isMusicPlayng
    if isPlaying and sound volume > 0:
        set originalVolume to sound volume
        set currentVolume to originalValue

        set fadeStepSize to currentVolume / fadeStepCount

        repeat while currentVolume > 0
            set currentVolume to currentVolume - fadeStepSize
            set sound volume to currentVolume
            delay fadeStepDuration
        end repeat

        set sound volume to 0
        delay fadeStepDuration
        set isMusicPlaying to false
        set sound volume to originalVolue
    end if
end tell
