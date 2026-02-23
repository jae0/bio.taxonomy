

  # taxonomy.db contains taxa/species codes that are internally (bio) consistent and parsimonious

  # require ( multicore ) # simple parallel interface (using threads) .. does not work well in MSWindows?

  require(aegis)
  require(worrms)

  project.library(  "aegis", "bio.taxonomy" )
   
  science_name = "Chionoecetes opilio"
  common_name = "snow crab"

  wm_records_name(name=science_name )[["valid_AphiaID"]]
  
  unique( wm_records_common(name=common_name)[["valid_AphiaID"]] )
  
  unique( wm_records_common(name=common_name)[["scientificname"]] )

  o = wm_records_taxamatch(name=science_name )
  
  # potentially multiple solutions possible for a given name .. result is a list
  unique( sapply(o, FUN=function(x) x[["valid_AphiaID"]] ) )
 
  aphiaid = wm_name2id(name = science_name)
  
  wm_id2name(id = aphiaid )

  wm_common_id(id = aphiaid )[["vernacular"]]  # vernacular name 

  wm_classification(id = aphiaid ) 
 
  unique( wm_synonyms(id = aphiaid )[["valid_AphiaID"]] )

  wm_synonyms(id = aphiaid )[["scientificname"]]
  
  # wm_attr_def(id = aphiaid)  # attributes

  o = bio.snowcrab::observer.db( DS="taxonomy" )
 
  o$common_name = o$COMMON
  o$scientific_name = o$SCIENTIFIC
  
  o = taxonomy_clean( o, "common_name" )
  o = taxonomy_clean( o, "scientific_name" )

  require(data.table)

  setDT(o)
  o$AphiaID = NA_integer_

  for (i in 1:nrow(o)) {
    
    if ( !is.na(o[i, AphiaID]) ) next()
    sn = o[i, scientific_name ]
    message( sn )
    if (!is.na(sn) & nchar(sn)>0 ) {
      if (sn=="reserved") next()
      asol = try( wm_name2id(name = sn), silent=TRUE )
      if (inherits(asol, "try-error") ) next()
      if ( length(asol) == 0 ) next()
      aid = unique( asol )
      if ( length(aid) == 1 ) {
        message( "id found from scientific name" )
        o[i, AphiaID := aid ]
      } else {
        message( "multiple ids found from scientific name" )
        message( asol )
      }
    }
  }

  for (i in 1:nrow(o)) {
    if ( !is.na(o[i, AphiaID]) ) next()
    cn = o[i, common_name ]
    message( cn )
    if (!is.na(cn) & nchar(cn)>0 ) {
      if (cn=="reserved") next()
      asol = try(  wm_records_common(name=cn), silent=TRUE )
      if (inherits(asol, "try-error") ) next()
      if ( length(asol) == 0 ) next()
      aid = unique( asol )
      if ( length(aid) == 1 ) {
        message( "id found from common name" )
        o[i, AphiaID := aid ]
      }
    }
  }
  




  require(ritis) # numerous similar methods 

  refresh.itis.tables = FALSE
  if ( refresh.itis.tables ) {
    itis.db( "make.snapshot", lnk="http://www.itis.gov/downloads/itisMySQLTables.tar.gz")
    itis.db( "main.redo")     # assemble all itis tables into a coherent and usable form
  }

  bootstrap.new.data.system = FALSE
  if ( bootstrap.new.data.system) {

    # first. refresh BIO's species codes from Oracle -- done also from groundfish update
    taxonomy.db( "spcodes.redo" )

    # bootstrap an initial set of tables .. these will be incomplete as a parsimonious tree needs to be created first but it depends upon the last file created taxonomy.db("complete") .. so ...
    taxonomy.db( "groundfish.itis.redo" )  ## link itis with groundfish tables using taxa names, vernacular, etc
    taxonomy.db( "full.taxonomy.redo" )  # merge full taxonomic hierrachy (limit to animalia and resolved to species)
    taxonomy.db( "life.history.redo" ) # add life history data (locally maintained in groundfish.lifehistory.manually.maintained.{csv/rda} )
    taxonomy.db( "complete.redo" )
    taxonomy.db( "parsimonious.redo" )
  }



### NOTE:: taxonomy db is creating wrong lookup for some species .. turtles  <<<<<<<<<<<<<<<<<<<

## TODO:: add Worms database
## links to observer database species codes




  example.usage = FALSE
  if (example.usage) {

    # -------------------------------
    # example usage to extract TSN's

		tx="Microgadus tomcod"

    tx="cod"

			taxonomy.recode( from="taxa.fast", tolookup=tx) # lookup only from local taxonomy db
			taxonomy.recode( from="taxa", tolookup=tx ) # look up species id from both itis and local taxonomy.db

      lu = taxonomy.recode( from="taxa", tolookup="AMMODYTES" ) # look up species id from both itis and local taxonomy.db
      taxonomy.recode( from="tsn", to="taxa", tolookup= lu[[1]]$tsn )

      taxonomy.recode( from="spec", to="taxa", tolookup=taxonomy.recode( from="taxa.fast", tolookup="AMMODYTES" ) )


      itis.taxa.to.tsn( tx) # look up only from itis

			o = taxonomy.recode( from="taxa", tolookup=tx )
				o
				taxonomy.recode( from="spec", tolookup=o[[1]]$spec )
				taxonomy.recode( from="tsn", to="taxa", tolookup= o[[1]]$tsn )
				itis.extract( o[[1]]$tsn[1], itis.db( "itaxa" ))


		taxonomy.recode( from="spec", tolookup=c(10,20) )

		itaxa = itis.db( "itaxa" )

		tx = "Microgadus tomcod"
			itis.taxa.to.tsn(  tx=tx, itaxa=itaxa )

    tx = "ling"
			txids = itis.vernacular.to.tsn( tx=tx, itaxa=itaxa )
      taxonomy.recode( from="tsn", to="taxa", tolookup=txids )

    # ITIS data from tsn's
    tsn = c(164714:164780)
			itis.extract( tsn, itaxa)

    # -------------------------------

    kid = itis.code.extract( DS="kingdom", value="Animalia")
    tid = itis.code.extract( DS="taxon.unit.types", value="species" )
    sid = itis.code.extract( DS="itaxa", value="Gadus morhua" )
    sid = itis.code.extract( DS="itaxa.vernacular", value="atlantic cod" )

  }
