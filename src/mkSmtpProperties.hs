cs = unlines [
  "module Smtp.SmtpProperties where",
  "",
  "myDomain = \"DomainName\"",
  "smtpHost = \"SmtpHostName\"",
  "fromName = \"FromName\"",
  "fromAddr = \"FromAddr\""
  ]

main = writeFile "Smtp/SmtpProperties.hs" cs
