# Praat script to calculate Mean Pitch
form Parameters
    sentence directory C:\Path\To\Your\SoundFiles\
    sentence extension .wav
    positive minimum_pitch 75
    positive maximum_pitch 600
endform

# Create a listing of all sound files
Create Strings as file list... list 'directory$'*'extension$'
numberFiles = Get number of strings

# Create a result file
header$ = "Filename'tab$'MeanPitch'newline$'"
fileappend "results.txt" 'header$'

# Iterate through all sound files
for ifile to numberFiles
    select Strings list
    filename$ = Get string... ifile
    Read from file... 'directory$''filename$'
    soundname$ = selected$ ("Sound", 1)
    
    # Calculate Pitch
    To Pitch... 0.01 'minimum_pitch' 'maximum_pitch'
    meanF0 = Get mean... 0 0 Hertz
    
    # Write result
    fileappend "results.txt" 'filename$''tab$''meanF0''newline$'
    
    # Clean up
    select Sound 'soundname$'
    plus Pitch 'soundname$'
    Remove
    select Strings list
endfor

printline Analysis complete. Results saved to results.txt