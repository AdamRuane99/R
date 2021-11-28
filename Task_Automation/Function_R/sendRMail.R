library(RDCOMClient)

sendRMail <- function(rmTo, rmCc, rmSubject, rmBody, rmAttachmentPath,
                      rmFilePath = "", rmDisc = TRUE) {
  
  

  OutApp <- COMCreate("Outlook.Application")
  
  ## create an email 
  outMail = OutApp$CreateItem(0)
  
  
  ## configure  email parameter 
  #rmFrom <- "TMsupport@mediolanum.ie" ### NOPE: there is no FROM.
  #outMail[["From"]] = rmFrom
  
  outMail[["To"]] = rmTo
  outMail[["Cc"]] = rmCc
  outMail[["subject"]] = rmSubject
  outMail[["HTMLBody"]] = paste0(rmBody, "<br>", "<br>", "\n\n", "-------------------------","\n")
  
  outMail[["attachments"]]$Add(rmAttachmentPath)
  
  if (length(rmFilePath) >= 1) { 
    for(attach in rmFilePath) {
      if(attach == "") next
      outMail[["Attachments"]]$Add("")
    }
  }
  
  ## send it                     
  outMail$Send()
  return("Thank you, your Mail has been sent succesfully!")
}
