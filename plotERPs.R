dPath = 'F:/CSL Choice/Data/EEG (raw data)/Visuals/'
fPrefix = 'N2pc'

conditions = c("epochInfo$Event==\"Search\" & epochInfo$LatStim==\"Target\" ",
               "epochInfo$Event==\"Search\" & epochInfo$LatStim==\"Distractor\" "
               )

rightHemCond = '& epochInfo$Hemifield==\"Right\" '
leftHemCond = '& epochInfo$Hemifield==\"Left\" '


eFilePattern = paste(fPrefix,"*_epochs.csv", sep="")
lFilePattern = paste(fPrefix,"*_LH.csv", sep="")
rFilePattern = paste(fPrefix,"*_RH.csv", sep="")
eFileList = list.files(dPath, pattern=glob2rx(eFilePattern))
lFileList = list.files(dPath, pattern=glob2rx(lFilePattern))
rFileList = list.files(dPath, pattern=glob2rx(rFilePattern))

for (subj in 1:length(eFileList)) {
  epochInfo = read.csv(file = paste(dPath,eFileList[subj], sep=""))
  lHemData = read.csv(file = paste(dPath,lFileList[subj], sep=""), header = FALSE)
  rHemData = read.csv(file = paste(dPath,rFileList[subj], sep=""), header = FALSE)
  
  for (cond in 1:length(conditions)) {
  #Extract epochs matching condition
  contralateral = cbind(lHemData[eval(parse(text=paste(conditions[cond], rightHemCond ,sep="")))],
                    rHemData[eval(parse(text=paste(conditions[cond], leftHemCond ,sep="")))]
                    )
  ipsilateral= cbind(lHemData[eval(parse(text=paste(conditions[cond], leftHemCond ,sep="")))],
                      rHemData[eval(parse(text=paste(conditions[cond], rightHemCond ,sep="")))]
                      )
  }
}
