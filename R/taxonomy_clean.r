
taxonomy_clean = function( X, vname, wds=NULL ) {
  
  X$flag = ""
  X[,vname] = tolower( X[,vname] )  

  # singleton characters
  X[,vname] = gsub( "\\<[[:alnum:]]{1}\\>", " ", X[,vname] , ignore.case=TRUE )  # remove ()/, etc.

  # words to remove -- with punctuation

  wds = unique( c( 
    "etc\\.", 
    "sps\\.", 
    "unid\\.", 
    "\\,unident\\.",
    "spp\\.", 
    "sp\\.", 
    "s\\.f\\.", 
    "sf\\.", 
    "s\\.p\\.", 
    "s\\.o\\.", 
    "so\\.", 
    "s\\.c\\.", 
    "\\(ns\\)",  
    "o\\.", 
    "f\\.", 
    "p\\.", 
    "c\\."
  ))
 
  for (w in wds) {
      ted = grep( w, X[,vname], ignore.case=TRUE) 
      if (length(ted) > 0 ) {
        wgr =  paste("[[:space:]]+", w, sep="")
        X[ted, vname] = gsub( wgr, "", X[ted, vname], ignore.case=TRUE ) 
        X[ted, "flag"] =  taxonomy.strip.unnecessary.characters( w )
      }
  } 
 
  # words to remove -- with no punctuation 
    wds2 = unique( c(
      "no", 
      "new", 
      "not as before", 
      "frozennnnn", 
      "unidentified",
      "unid", 
      "adult", 
      "SUBORDER", 
      "ORDER", 
      "etc", 
      "sp", 
      "spp", 
      "so", 
      "ns",  
      "c", 
      "p", 
      "o",
      "f", 
      "s", 
      "perhaps", 
      "empty shells", 
      "digested",
      "unknown", 
      "pink lump", 
      "frozen", 
      "shells scal",
      "unidentified", 
      "saved for identification", 
      "unident", 
      "saved for id", 
      "maybe", 
      "arc",
      "eggs", 
      "egg", 
      "reserved",  
      "remains", 
      "debris",  
      "stones", 
      "mucus", 
      "bait", 
      "foreign", 
      "fluid", 
      "operculum", 
      "crude", 
      "water", 
      "Unidentified Per Set", 
      "Unidentified Species", 
      "Unid Fish And Invertebrates", 
      "Parasites", 
      "Groundfish", 
      "Marine Invertebrates",
      "Sand Tube",
      "invert unsp",
      "obsolete", 
      "berried", 
      "short", 
      "larvae", 
      "larval", 
      "order",
      "egg",
      "eggs",
      "empty",
      "berried",
      "live",
      "megalops",
      "purse",
      "juvenile" 
  ))  

  for (w in wds2) {
    ted = grep( w, X[,vname], ignore.case=TRUE) 
    if (length(ted) > 0 ) {
      wgr =  paste("\\<", w, "\\>", sep="")
      X[ted, vname] = gsub( wgr, "", X[ted, vname], ignore.case=TRUE )
      X[ted, "flag"] =  taxonomy.strip.unnecessary.characters( w )
    }
  }

  X[,vname] = taxonomy.strip.unnecessary.characters( X[,vname] )
  
  # rename
  X[,vname] = gsub("ATL ", "Atlantic", X[,vname], ignore.case=TRUE  )
 

  X[,"flag"] = taxonomy.strip.unnecessary.characters( X[,"flag"] )
  X$flag[ X$flag=="egg"] = "eggs"

  X$flag[ X$flag=="ns"] = "unidentified"
  X$flag[ X$flag=="unid"] = "unidentified"
  X$flag[ X$flag=="unident"] = "unidentified" 


  to_ignore = unique(c(
      "no", 
      "new", 
      "not as before", 
      "frozennnnn", 
      "perhaps", 
      "empty shells", 
      "unknown", 
      "pink lump", 
      "frozen", 
      "shells scal",
      "unidentified", 
      "saved for identification", 
      "saved for id", 
      "maybe", 
      "arc",
      "obsolete",      
      "reserved",  
      "remains", 
      "debris",  
      "stones", 
      "mucus", 
      "bait", 
      "foreign", 
      "fluid", 
      "operculum", 
      "crude", 
      "water", 
      "Unidentified Per Set", 
      "Unidentified Species", 
      "Unid Fish And Invertebrates", 
      "Parasites", 
      "Groundfish", 
      "Marine Invertebrates",
      "Sand Tube",
      "invert unsp"
  ))

  X$tolookup  = TRUE
  X$tolookup[ which( X$flag %in% to_ignore) ] = FALSE
  
  return (X)
}


