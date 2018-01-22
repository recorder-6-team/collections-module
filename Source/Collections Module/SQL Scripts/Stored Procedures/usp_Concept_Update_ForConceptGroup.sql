If EXISTS (
	SELECT * FROM SysObjects 
	WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_Update_ForConceptGroup]') 
	AND	  OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concept_Update_ForConceptGroup]
GO

/*===========================================================================*\
  Description:	update the domain mask when user moving a Concept to different 
				Concept Group, then it is necessary to recalulate the entire 
				domain structure of the collection database for all relate date

  Parameters:
	@ParentKey	Concept_Group_Key
	@ChildKey	Concept_Key
				

 Created:	Qing Sun	28/11/2008

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Concept_Update_ForConceptGroup] 
	@ParentKey	CHAR(16),
	@ChildKey	CHAR(16)
AS
	SET NOCOUNT ON
	DECLARE		@OldDomainMask INT,
				@NewDomainMask INT,
				@SpecimenCollectionUnitKey CHAR(16)

	-- get old domain mask
	SELECT		@OldDomainMask		 =  D.Domain_Mask
	FROM		Concept	C
	JOIN		Concept_Group	CG	 ON C.Concept_Group_Key = CG.Concept_Group_Key
	JOIN		Local_Domain	LD	 ON	CG.Local_Domain_Key	= LD.Local_Domain_Key
	JOIN		Domain			D	 ON	LD.Domain_Key		= D.Domain_Key
	WHERE		C.Concept_Key		 =  @ChildKey
	-- update the concept group key
	UPDATE		Concept
	SET			Concept_Group_Key	 = @ParentKey
	WHERE		Concept_Key			 = @ChildKey	

	-- get new domain mask
	SELECT		@NewDomainMask		 =  D.Domain_Mask
	FROM		Concept	C
	JOIN		Concept_Group	CG	 ON C.Concept_Group_Key = CG.Concept_Group_Key
	JOIN		Local_Domain	LD	 ON	CG.Local_Domain_Key	= LD.Local_Domain_Key
	JOIN		Domain			D	 ON	LD.Domain_Key		= D.Domain_Key
	WHERE		C.Concept_Key		 =  @ChildKey

	IF (@NewDomainMask <> @OldDomainMask) 
	BEGIN						
	-- Declare to find the distinct specimen domains
		DECLARE csr CURSOR FOR
			SELECT 	SU.Collection_Unit_Key		
			FROM 	Specimen_Unit	SU
			JOIN	Determination	D 	WITH (INDEX (IX_Determination_Specimen)) 
										ON	 D.Determination_Key	=	SU.Preferred_Determination_Key
			JOIN	Concept			C 	WITH (INDEX (PK_Concept)) 
										ON	 C.Concept_Key			=	D.Concept_Key
			JOIN	Concept_Group   CG 	ON	 CG.Concept_Group_Key	=	C.Concept_Group_Key
			JOIN	Local_Domain    LD 	ON	 LD.Local_Domain_Key	=	CG.Local_Domain_Key
			JOIN	Domain          DM 	ON	 DM.Domain_Key			=	LD.Domain_Key
										AND	 LD.Local_Domain_Key	=	@ChildKey	
		OPEN csr
		
		FETCH NEXT FROM csr INTO @SpecimenCollectionUnitKey

		WHILE (@@FETCH_STATUS = 0)
		BEGIN
			EXECUTE	[dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] @SpecimenCollectionUnitKey, @OldDomainMask, @NewDomainMask
			-- get next key
			FETCH NEXT FROM csr INTO @SpecimenCollectionUnitKey
		END
		
		CLOSE csr

		DEALLOCATE csr
	END

	SET NOCOUNT OFF
GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('[dbo].[usp_Concept_Update_ForConceptGroup]') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure [usp_Concept_Update_ForConceptGroup]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON [dbo].[usp_Concept_Update_ForConceptGroup] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_Concept_Update_ForConceptGroup] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[usp_Concept_Update_ForConceptGroup] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[usp_Concept_Update_ForConceptGroup] TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON [dbo].[usp_Concept_Update_ForConceptGroup] TO [Dev - JNCC SQL]
END

GO