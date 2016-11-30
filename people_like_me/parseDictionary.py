# parse data dictionary into something R can use
# since R is very slow at parsing this much text, using python
dictFile = "/home/eli/Dropbox/Code/ACS/people_like_me/smallDataDict14.txt"

# read and parse the data dictionary 
f = open(dictFile, "r")

allText = f.read()
allEntries = allText.split("\n\n")

possCats = []
varSwitch = []
for entry in allEntries:
  thisEntry = entry.split("\n")

  nameDB = thisEntry[0][:-2] # strip off number at end of name
  nameDB = nameDB.lower()
  if (nameDB=='type'): nameDB = 'type_' # special case

  nameHuman = thisEntry[1]

  possCat = "'" + nameHuman + "'='" + nameDB + "'"
  possCats.append(possCat)

  optionList = thisEntry[2:]
  possVars = nameDB + "= {possible.values <- c("
  for option in optionList:
    response = option.split(" .")
    if (len(response) < 2): # this is a numeric category
      possVar = "text field" # this acts weird
    else:
      possVar = "'" + response[1] + "'='" + response[0] + "'"
      possVars += possVar + ","
  possVars = possVars[:-1] + ")}"
  varSwitch.append(possVars)

f.close()



# write out R code
f = open("newPossibleCategories.R", "w")

f.write("possible.categories <- c(\n")
for cat in possCats:
  f.write(cat + ",\n")
f.write(")\n")

f.write("\n")

f.write("possible.values <- c('')\n")

f.write("\n")

f.write("getPossibleValues <- function(colname){\n")
f.write("switch(colname,\n")
for var in varSwitch:
  f.write(var + ",\n")
f.write(")\n")
f.write("return(possible.values)\n")
f.write("}")


f.close()











