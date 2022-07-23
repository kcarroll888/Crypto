############################################
#   SEND EMAIL NOTIFICATION ON BREAKOUTS   #
############################################

library(mailR)

yhPwd <- read.csv("C:/Users/kurtc/Google Drive/Crypto/YahooPwd.csv", stringsAsFactors = F)
emailTitle <- paste("Daily Breakouts for", Sys.Date())
filePathName <- paste0("C:/Users/kurtc/Google Drive/Crypto/Breakouts/", Sys.Date(), " DailyBreakouts.csv")

# Send the email
send.mail(from = "kurt.carroll@rocketmail.com",
          to = "kcarroll888@gmail.com",
          subject = emailTitle,
          body = emailTitle,
          smtp = list(host.name = "smtp.mail.yahoo.com",
                      port = 465, user.name = "kurt.carroll@rocketmail.com",
                      passwd = yhPwd[1,1], ssl = TRUE),
          authenticate = TRUE, send = TRUE,
          attach.files = filePathName)
