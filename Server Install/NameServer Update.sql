/*Update the NameServer table for the existing Lists*/
	DECLARE	@taxon_version_Wellformed			Char(1),
				@taxon_version_Incorrectformed		Char(1),
				@taxon_version_Unverified			Char(1),
				@taxon_version_status_recommended	Char(1),
				@taxon_version_status_synonym		Char(1),
				@taxon_type_scientific 				Char(1),
				@taxon_type_vernacular  			Char(1),
				@taxon_name_type_key_formal			CHAR(16)
	
	SET			@taxon_version_wellformed			=	'W'	
	SET			@taxon_version_incorrectformed		=	'I'
	SET			@taxon_version_unverified			=	'U'			
    SET			@taxon_version_status_recommended	=	'R'		
	SET			@taxon_version_status_synonym		=	'S'
	SET			@taxon_type_scientific				=	'S'
	SET			@taxon_type_vernacular				=	'V'
	SET			@taxon_name_type_key_formal			=	'MNHSYS0000000001'

	UPDATE NS 
	SET Taxon_Version_FORM				=	CASE WHEN TV1.Validation_Level = 0 THEN @taxon_version_wellformed
												 WHEN TV1.Validation_Level = 3 THEN @taxon_version_incorrectformed 
												 ELSE @taxon_version_unverified 
											END,
		Taxon_Version_STATUS			=	CASE WHEN TV1.Taxon_Version_Key is NULL THEN @taxon_version_unverified 
												 WHEN TV1.Taxon_Version_Key  = TLI.Taxon_Version_Key THEN @taxon_version_status_recommended 
												 ELSE @taxon_version_status_synonym  
											END,
		Taxon_Type						=   CASE WHEN TNT.TAXON_NAME_TYPE_KEY = @taxon_name_type_key_formal THEN @taxon_type_scientific 
												 ELSE @taxon_type_vernacular 
											END, 
		Recommended_Taxon_Version_Key	=	TV1.Taxon_Version_Key,
		Recommended_Taxon_List_Item_Key =	TLI.PREFERRED_NAME,
		Entered_By						=	TLI.ENTERED_BY,
		Entry_Date						=	TLI.ENTRY_DATE,
		Changed_By						=	TLI.CHANGED_BY,
		Changed_Date					=	TLI.CHANGED_DATE
	FROM NameServer NS 
	INNER JOIN Taxon_List_Item		TLI	 ON NS.INPUT_Taxon_Version_Key	=	TLI.Taxon_Version_Key
	INNER JOIN Taxon_List_Version	TLV	 ON TLV.Taxon_List_Version_Key	=	TLI.Taxon_List_Version_Key
	LEFT JOIN	Taxon_List_Item		TLI1 ON TLI1.Taxon_List_Item_Key	=	TLI.Preferred_Name
	LEFT JOIN	Taxon_Version		TV1  ON TV1.Taxon_Version_Key		=	TLI1.Taxon_Version_Key 
	INNER JOIN	TAXON				TX	 ON TX.TAXON_KEY				=	TV1.TAXON_KEY
	INNER JOIN	TAXON_NAME_TYPE		TNT  ON TNT.TAXON_NAME_TYPE_KEY		=	TX.TAXON_NAME_TYPE_KEY
