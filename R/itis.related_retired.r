  itis.related = function( DS ) {

    taxadir = project.datadirectory( "bio.taxonomy", "data" )
    dir.create( taxadir, recursive=TRUE, showWarnings=FALSE )

    if (DS %in% c( "itis.oracle", "itis.oracle.redo" ) ) {

      ### NOT USED ??? TO DELETE ?

      fn.itis = file.path( taxadir, "itis.oracle.rdz" )
      if (DS=="itis.oracle" ) {
        itis = read_write_fast( fn.itis )
        return (itis)
      }
      itis.groundfish = taxonomy.db( "itis.groundfish.redo" )
      itis.observer = taxonomy.db( "itis.observer.redo" )
      toextract = colnames( itis.observer)  # remove a few unuses vars
      itis = rbind( itis.groundfish[,toextract] , itis.observer[,toextract] )
      ii = which(duplicated( itis$given_spec_code ) )
      itis = itis[ -ii, ]
      read_write_fast(itis, file=fn.itis)
      return (itis)
    }

    if (DS %in% c( "itis.groundfish", "itis.groundfish.redo" ) ) {


      ### NOT USED ??? TO DELETE ?

      fn.itis = file.path( taxadir, "itis.groundfish.rdz" )
      if (DS=="itis.groundfish" ) {
        itis = read_write_fast( fn.itis )
        return (itis)
      }
      require(RODBC)
      connect=odbcConnect( oracle.taxonomy.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      itis = sqlQuery(connect, "select * from groundfish.itis_gs_taxon")
      odbcClose(connect)
      names(itis) =  tolower( names( itis ) )
      for (i in names(itis) ) itis[,i] = as.character( itis[,i] )
      read_write_fast(itis, file=fn.itis )
      return (itis)
    }

    if (DS %in% c( "itis.observer", "itis.observer.redo" ) ) {


      ### NOT USED ??? TO DELETE ?

      fn.itis = file.path( taxadir, "itis.observer.rdz" )
      if (DS=="itis.observer" ) {
        itis = read_write_fast( fn.itis )
        return (itis)
      }
      require(RODBC)
      connect=odbcConnect( oracle.taxonomy.server, uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F)
      itis = sqlQuery(connect, "select * from observer.itis_isdb_species")
      odbcClose(connect)
      names(itis) =  tolower( names( itis ) )
      for (i in names(itis) ) itis[,i] = as.character( itis[,i] )
      read_write_fast(itis, file=fn.itis)
      return (itis)
    }

  }

