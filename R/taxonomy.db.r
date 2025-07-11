

  taxonomy.db = function( DS="complete", itis.taxa.lowest="species" ) {

    taxadir = file.path(project.datadirectory( "bio.taxonomy"), "data" )
    localdir = file.path(project.datadirectory( "bio.taxonomy"), "data", "data.locally.generated" )

    dir.create( taxadir, recursive=TRUE, showWarnings=FALSE )
    dir.create( localdir, recursive=TRUE, showWarnings=FALSE )

    if ( DS == "gstaxa" ) return( taxonomy.db( "life.history") )

    if ( DS %in% c("spcodes", "spcodes.redo"  ) ) {
      # this is just a synonym for the groundfish data dump :
      if ( DS == "spcodes" ) {
        spcodes = groundfish_survey_db( DS="spcodes.rawdata" )
        return( spcodes )
      }

      groundfish_survey_db( DS="spcodes.rawdata.redo" )
      return ( "Complete" )
    }



    # ------------------------------



    if (DS %in% c("groundfish.itis.redo", "groundfish.itis" )) {

			print( "Warning:  ")
      print( "  spec = bio species codes -- use this to match data but not analysis" )
			print( "  spec.clean = manually updated codes to use for taxonomic work " )
			print( "" )

			# add itis tsn's to spcodes -- this completes the lookup table
			# a partial lookup table exists and is maintained locally but then is added to using
			# text matching methods, which are a bit slow.

      fn = file.path( localdir, "lookup.groundfish.itis.rdz" )

      if ( DS =="groundfish.itis") {
        spi = NULL
        if (file.exists(fn)) spi=read_write_fast(fn)
        return (spi)
      }

			print( "Merging ITIS and Groundfish species codes using taxa or vernacular names ...")

      # load groundfish species codes
      spi = taxonomy.db( DS="spcodes" )
      names(spi) = tolower( names(spi) )

      spi$name.common = as.character( spi$comm )
      spi$name.scientific = as.character( spi$spec )
      spi$spec = as.numeric(spi$code)
      spi = spi[ , c("spec", "name.common", "name.scientific" ) ]

      spi$itis.tsn = spi$spec.clean = spi$comments = NA

      # additional vars to help with lookup
      spi$tolookup = TRUE
      spi$flag = ""


      # items to drop identified in groundfish.itis.lookuptable.manually.maintained.csv
			# mark items to not be looked up
			ib = which( spi$itis.tsn == -1  )
			if (length (ib)>0 ) {
				spi$tolookup[ ib] = FALSE
				spi$flag[ib] = "manually rejected"
				spi$itis.tsn[ ib] = NA  # reset to NA
			}

			lu = which( is.finite(spi$itis.tsn))
			spi$tolookup[lu] = FALSE
			spi$flag[lu] = "manually determined"


      # check for keywords that flag that no lookup is necessary
      spi = taxonomy.keywords.flag( spi, "name.scientific" )
      spi = taxonomy.keywords.flag( spi, "name.common" )

      # remove words with punctuation
      spi = taxonomy.keywords.remove( spi, "name.scientific", withpunctuation=T )
      spi = taxonomy.keywords.remove( spi, "name.common", withpunctuation=T )

      # remove words without punctuation
      spi = taxonomy.keywords.remove( spi, "name.scientific", withpunctuation=F )
      spi = taxonomy.keywords.remove( spi, "name.common", withpunctuation=F )

      # final formatting of names
      spi$name.scientific = taxonomy.strip.unnecessary.characters(spi$name.scientific)
      spi$name.common = taxonomy.strip.unnecessary.characters(spi$name.common)


      # link itis with groundfish species codes using an exhaustive search of all taxa names
      vnames = c( "name.scientific", "name.scientific", "name.common")
      vtypes = c( "default", "vernacular", "vernacular", "default" )
      spi = itis.lookup.exhaustive.search( spi, vnames, vtypes )


      # fill in missing names, etc
      i = which(is.na( spi$name.scientific))
      if (length(i) > 0 ) {
        oo = taxonomy.recode( from="tsn", to="sci", tolookup=spi$itis.tsn[i] )
        if (length(oo) == length(i)) spi$name.scientific[i] = oo
      }

      i = which(is.na( spi$name.common))
      if (length(i) > 0 ) {
        oo = taxonomy.recode( from="tsn", to="tx", tolookup=spi$itis.tsn[i] )
        if (length(oo) == length(i)) spi$name.common[i] = oo
      }

      # have to do it again and fill with scientific name if missing
      i = which(is.na( spi$name.common))
      if (length(i) > 0 ) {
        oo = taxonomy.recode( from="tsn", to="sci", tolookup=spi$itis.tsn[i] )
        if (length(oo) == length(i)) spi$name.common[i] = oo
      }

  		# make sure the remainder of missing spec.clean points to spec
			i = which( !is.finite(spi$spec.clean) )
			if (length(i)>0) {
        spi$spec.clean[i] = spi$spec[i]
      }

  		# for all duplicated tsn's, point them to the same species id:
      # spec.clean id's to point to a single spec id (choose min value as default)
		  dd = 	which(spi$itis.tsn %in% spi$itis.tsn[which(duplicated(spi$itis.tsn))]) # all dups
      dup.tsn = sort( unique( spi$itis.tsn[ dd ]  ))
			if (length( dup.tsn) > 0 ) {
        for (tsni in dup.tsn) {
          oi = which( spi$itis.tsn ==tsni )
          op = which.min( spi$spec[ oi ] )
          spi$spec.clean[oi] = min( spi$spec[oi[op]],  spi$spec[oi] )
        }
      }

      i = which( !is.finite(spi$itis.tsn) & spi$tolookup )
			if (length(i)>0) {


      # as of 27 Feb 2015, the following have known issues of having no itis matches

# spi[i,]
#    spec          name.common      name.scientific spec.clean     comments
#306  3163            LEIOCHONE            LEIOCHONE       3163    NA
#625  6700     PSOLUSES THYONES     PSOLUSES THYONES       6700    Species not found: common ancestor Dendrochirotida
#1344 2865       PONTOGENEIIDAE       PONTOGENEIIDAE       2865    NA
#1830 1800        PROTOCHORDATA        PROTOCHORDATA       1800    NA
#1848 1920 BRYOZOANS ECTOPROCTA BRYOZOANS ECTOPROCTA       1920    NA
#1956 8200         COELENTERATA         COELENTERATA       8200    Obsolete: Cnidaria and Ctenophora, using latter
#2133 8363 HALIPTERUS BALTICINA HALIPTERUS BALTICINA       8363    Species not found, taken as genus


# as of 07 Feb 2020, the following have known issues of having no itis matches
#  spec     name.common name.scientific comments spec.clean itis.tsn tolookup flag
# 2383 8392   HETEROPOLYPUS   HETEROPOLYPUS     <NA>       8392       NA     TRUE
# 2413 2796 MEGALANCEOLIDAE MEGALANCEOLIDAE     <NA>       2796       NA     TRUE
# 2428 2580   PENTACHELIDAE   PENTACHELIDAE     <NA>       2580       NA     TRUE
# 2439 2505      POLYBIIDAE      POLYBIIDAE     <NA>       2505       NA     TRUE
# overrides are placed here:


        # overrides are placed here:
        known.issues.spec = c(  3163,   6700,  2865,   1800,   1920,   8200,
           8363, 8392, 2796, 2580, 2505)  # add new unmatched "spec" here
        known.issues.tsn  = c( 67602, 158142, 93681, 203347, 155470, 118845, 719025,
           1812, 609975, 97683, 206959)  # manually determined tsn's here
        known.issues.comments = c( "", "Species not found: common ancestor Dendrochirotida", "", "", "", "Obsolete: Cnidaria and Ctenophora, using latter",
          "Species not found: genus", "Species not found, Using family Alcyoniidae", "Species not found, Using family Lanceoloidea", "Probably miscoded, assuming Penta, Using polychelidae", "Polybiidea fall under Potunoidea" )

        for ( pp in 1:length( known.issues.spec) ) {
          kk = known.issues.spec[pp]
          oo = which( spi$spec==kk)
          if (length(oo)==1) {
            spi$itis.tsn[oo] = known.issues.tsn[pp]
            spi$comments[oo] = known.issues.comments[pp]
          }
        }

        jj = which( !is.finite(spi$itis.tsn) & spi$tolookup   )

        if (length(jj) > 0 ) {
          # should not be necessary unles taxa list at rawdata level has changed (e.g., due to new species inclusions )
          print( "The following species have no itis tsn matches. ")
	  			print( "Their tsn's should be manually identified and workarounds should be placed in this" )
          print( "function, 'taxonomy.db(DS='groundfish.itis.redo')' " )
				  print( spi[jj,] )
			    stop()
        }
      }
      read_write_fast( spi, file=fn

      return ( fn )
    }


    # ------------------

    if (DS %in% c("full.taxonomy", "full.taxonomy.redo") ) {

      # add full taxonomic hierarchy to spcodes database ..

      require ( parallel ) # simple parallel interface (using threads)

      itis.taxa.lowest = tolower(itis.taxa.lowest)
      fn = file.path( localdir, paste("spcodes.full.taxonomy", itis.taxa.lowest, "rdz", sep=".") )

      if (DS=="full.taxonomy") {
        spf = NULL
        if (file.exists(fn)) spf = read_write_fast(fn)
        return(spf)
      }

      spf = taxonomy.db( DS="groundfish.itis" )
      itaxa = itis.db( "itaxa" )
      tunits =  itis.db( "taxon.unit.types" )
      kingdom =  itis.db( "kingdoms" )

      # reduce memory requirements: drop information below some taxonomic level
      tx_id = unique( tunits$rank_id[ which( tolower(tunits$rank_name)==itis.taxa.lowest ) ] )
      tunits = tunits[ which( tunits$rank_id <= tx_id ) , ]
			tunits = tunits[ -which(duplicated(tunits$rank_id)) ,]
			tunits = tunits[ order( tunits$rank_id) , ]
      tunits = merge( tunits, kingdom, by="kingdom_id", sort=F )


      fd = which( duplicated ( tunits$rank_name) )
      if ( length(fd) > 0 ) {
        for (fi in fd ) {
          d0 = which( tunits$rank_name == tunits$rank_name[ fi] )
          tunits$rank_name[d0] = paste( tunits$kingdom_name[d0], tunits$rank_name[d0],sep=".")
        }
      }

      test = NULL
      while ( is.null( test) | is.null(names(test)) ) {
        test =  itis.format( sample(nrow(spf), 1), tsn=spf$itis.tsn, itaxa=itaxa, tunits=tunits )
      }
      formatted.names = names( test )

			debug = F
			if (debug) {
				out = matrix(NA, ncol=length(test), nrow=nrow(spf) )
				for ( i in 1:nrow(spf) ) {
					o = itis.format( i, tsn=spf$itis.tsn, itaxa=itaxa, tunits=tunits )
					print (o)
					out[i,] = o
				}
			}

      print( "Extracting full taxonomy" )
      res = list()
      for (i in 1:nrow(spf) ) {
        print(i)
        res[[i]] = itis.format( i=i, tsn=spf$itis.tsn, itaxa=itaxa, tunits=tunits )
      }

      res = unlist( res)
      res = as.data.frame( matrix( res, nrow=nrow(spf), ncol=length(formatted.names), byrow=T ), stringsAsFactors=F )
      colnames(res) = tolower(formatted.names)
      res = res[order(res$rowindex ) ,]

      # mclapply method
			# res = mclapply( 1:nrow(spf), itis.format, tsn=spf$itis.tsn, itaxa=itaxa, tunits=tunits )
      # res = unlist( res)
      # res = as.data.frame( matrix( res, nrow=nrow(spf), ncol=length(formatted.names), byrow=T ), stringsAsFactors=F )
      # colnames(res) = tolower(formatted.names)

      spf = cbind( spf, res )
      spf$rowindex = NULL

      read_write_fast( spf, file=fn )
      return ( fn )

    }



    # ------------------


    if (DS %in% c( "life.history", "life.history.redo") ) {

      fn = file.path( localdir, "spcodes.lifehistory.rdz")

      fn.local = system.file("extdata", "groundfish.lifehistory.manually.maintained.csv", package = "bio.taxonomy")

      if (DS == "life.history" ) {
        sps = NULL
        if (file.exists(fn)) sps = read_write_fast(fn)
        return(sps)
      }

      #print( "Local files are manually maintained.")
      #print( "Export to CSV if they have been updated to with '|' as a delimiter and remove last xx lines:" )
      #print( fn.local )

      lifehist = read.csv( fn.local, sep="|", as.is=T, strip.white=T, header=T, stringsAsFactors=F )
      names( lifehist ) = tolower( names( lifehist ) )
			lifehist$spec = as.numeric( as.character( lifehist$spec ))

			lifehist = lifehist[ - which( duplicated ( lifehist$spec ) ) , ]
			lifehist = lifehist[ which( is.finite( lifehist$spec ) ) , ]


      itis.taxa.lowest = tolower(itis.taxa.lowest)

      sps = taxonomy.db( DS="full.taxonomy", itis.taxa.lowest="species" )
      sps = merge( sps, lifehist, by="spec", all.x=T, all.y=T, sort=F)

      sps$rank_id = as.numeric(  sps$rank_id  )

      sps$end = NULL  # dummy to force/check correct CVS import
      sps$subgenus = NULL
      sps$tribe = NULL

			ii = which( is.na( sps$name.scientific ) )
      sps$name.scientific[ii] = sps$name.common.worktable[ii]

			ii = which( is.na( sps$name.scientific ) )
      sps$name.scientific[ii] = sps$vernacular[ii]


			ii = which( is.na( sps$name.common ) )
			sps$name.common[ii] = sps$vernacular[ii]

			ii = which( is.na( sps$name.common ) )
			sps$name.common[ii] = sps$name.common.worktable[ii]

			ii = which( is.na( sps$name.scientific ) )
      sps$name.scientific[ii] = sps$name.common[ii]

			ii = which( is.na( sps$name.common ) )
      sps$name.common[ii] = sps$name.scientific[ii]

      last.check =  grep("per set", sps$name.common, ignore.case=T)
      if (length(last.check)>0) sps = sps[ -last.check , ]


      read_write_fast( sps, file=fn )
      return( fn )

    }


    # ----------------------------------------------

    if ( DS %in% c("complete", "complete.redo") ) {
		  fn = file.path( localdir, "spcodes.complete.rdz")
  	  sps = NULL
			if (DS == "complete" ) {
        if (file.exists(fn)) sps = read_write_fast(fn)
        return(sps)
      }

			sp = NULL

			tx = taxonomy.db("life.history")

			spec = sort( unique( tx$spec) )

			sp.graph = network.igraph( spec=spec, tx=tx )
			sp.graph = network.statistics.igraph( g=sp.graph )  # add some stats about children, etc.
      spi = network.igraph.unpack( g=sp.graph )

      # go through the list of taxa and identify updated spec.clean id's
			n0 = length(spec)
			sp = data.frame( spec=spec )
			sp = merge( sp, tx, by="spec", all.x=T, all.y=F )
			jj = which( !is.finite( sp$spec.clean ) )
			sp$spec.clean[jj] = sp$spec[jj]

			sps = merge( sp, spi[,c("id", "usage", "children.n","children", "sci", "parent" )],
  			by.x="itis.tsn", by.y="id", all.x=T, all.y=F)

			read_write_fast (sps, file=fn)
			return ( fn )
		}


    # ----------------------------------------------



    if (DS %in% c( "parsimonious",  "parsimonious.redo" )) {

      fn = file.path( localdir, "spcodes.parsimonious.rdz" )

      if ( DS =="parsimonious") {
 		    # determine the most parsimonious species list based upon know taxonomy/phylogeny and local species lists
  	    spi = NULL
        if (file.exists(fn)) spi = read_write_fast(fn)
        return(spi)
      }

      # load groundfish species codes
      spi = taxonomy.db("complete")
      spi$spec.parsimonious = spi$spec.clean  # initialize with current best codes which are found in spec.clean

      ranks = sort( unique( spi$rank_id ),decreasing=T )
      ranks = setdiff( ranks, 220 )  # 220 is the lowest level of discrimination (species)

      # search for single children and a parent, recode parent to child
      for ( r in ranks ) {
        oo = which( spi$rank_id == r )
        for ( o in oo ) {
          if ( is.finite(spi$children.n[o]) && spi$children.n[o] == 1) {
            # only child --> recode this spec to child's spec
            newspec = which( spi$itis.tsn == spi$children[o] ) # multiple matches likely as this is a recursive process -- pick lowest taxa level == highest rank_id
            if (length( newspec) == 0 ) next()
            if (!is.finite( spi$rank_id[newspec] ) ) next()
            nsp = newspec[which.max( spi$rank_id[newspec] )]
            if ( o==nsp ) next()
            spi$spec.parsimonious[o] = spi$spec.parsimonious[nsp]
            spi$children[o] = spi$children[nsp]
            spi$children.n[o] = spi$children.n[nsp]
            print( paste(  "Updating species list::", spi$spec.parsimonious[o], spi$sci[o], "->", spi$spec.parsimonious[nsp], spi$sci[nsp] ) )
          }
        }
      }

      read_write_fast( spi, file=fn )

      return ( fn )
    }

  }


