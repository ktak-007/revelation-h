module Error where

type Msg = String

magicString :: Msg
magicString = "Invalid magic string"

format :: Msg
format = "Invalid file format"

version :: Msg
version = "Unknown data version"

initialVector :: Msg
initialVector = "Initial vector Error"

password :: Msg
password = "Password Error"

hash256 :: Msg
hash256 = "Hash256 Error"

cipherInit :: Msg
cipherInit = "Cipher init Error"

xmlUnknownEntryType :: Msg
xmlUnknownEntryType = "Unknown entry type"

xmlNameNotFound :: Msg
xmlNameNotFound = "XML Entry without \"name\" element"

xmlNameMoreThanOnce :: Msg
xmlNameMoreThanOnce = "XML Entry with several \"name\" elements"

xmlElementIsAbsent:: Msg
xmlElementIsAbsent = "XML Element is mandatory"

xmlElementIsNotSingle :: Msg
xmlElementIsNotSingle = "XML Element is not single"

xmlAttributeIsAbsent:: Msg
xmlAttributeIsAbsent = "XML Attribute is mandatory"

xmlAttributeIsNotSingle :: Msg
xmlAttributeIsNotSingle = "XML Attribute is not single"
