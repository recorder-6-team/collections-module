If EXISTS (
	SELECT * FROM SysObjects 
	WHERE Id = OBJECT_ID(N'[dbo].[usp_ConceptGroup_Update_ForLocalDomain]') 
	AND	  OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConceptGroup_Update_ForLocalDomain]
GO

/*===========================================================================*\
  Description:	update the domain mask when user moving a ConceptGroup to different 
				Local Domain, then it is necessary to recalulate the entire 
				domain structure of the collection database for all relate date

  Parameters:
	@ParentKey	Local_Domain_Key 
	@ChildKey	Concept_Group_Key	
 

 Created:	Qing Sun	28/11/2008

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ConceptGroup_Update_ForLocalDomain] 
	@ParentKey	CHAR(16),
	@ChildKey	CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@OldDomainMask INT,
				@NewDomainMask INT,
				@SpecimenCollectionUnitKey CHAR(16)

	-- get the old domain mask
	SELECT		@OldDomainMask		 =  D.Domain_Mask
	FROM		Concept_Group	CG
	JOIN		Local_Domain	LD	 ON	CG.Local_Domain_Key	= LD.Local_Domain_Key
	JOIN		Domain			D	 ON	LD.Domain_Key		= D.Domain_Key
	WHERE		CG.Concept_Group_Key =  @ChildKey

	UPDATE		Concept_Group
	SET			Local_Domain_Key	 = @ParentKey
	WHERE		Concept_Group_Key	 = @ChildKey	

	-- get new domain mask
	SELECT		@NewDomainMask		 =  D.Domain_Mask
	FROM		Concept_Group	CG
	JOIN		Local_Domain	LD	 ON	CG.Local_Domain_Key	= LD.Local_Domain_Key
	JOIN		Domain			D	 ON	LD.Domain_Key		= D.Domain_Key
	WHERE		CG.Concept_Group_Key =  @ChildKey   

	IF (@NewDomainMask <> @OldDomainMask) 
	BEGIN						
	-- Declare to find the distinct specimen domains
		DECLARE csr CURSOR FOR
			SELECT 	SU.Collection_Unit_Key		
			FROM 	Specimen_Unit		SU
			JOIN	Determination		D 	WITH (INDEX (IX_Determination_Specimen)) 
											ON	 D.Determination_Key	=	SU.Preferred_Determination_Key
			JOIN	Concept				C 	WITH (INDEX (PK_Concept)) 
											ON	 C.Concept_Key			=	D.Concept_Key
			JOIN	Concept_Group		CG 	ON	 CG.Concept_Group_Key	=	C.Concept_Group_Key
			JOIN	Local_Domain		LD 	ON	 LD.Local_Domain_Key	=	CG.Local_Domain_Key
			JOIN	Domain				DM 	ON	 DM.Domain_Key			=	LD.Domain_Key
											AND	 CG.Concept_Group_Key	=	@ChildKey		
		OPEN csr
		
		FETCH NEXT FROM csr INTO @SpecimenCollectionUnitKey
		-- while loop start
		WHILE (@@FETCH_STATUS = 0)
		BEGIN
			EXECUTE	[dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] @SpecimenCollectionUnitKey, @OldDomainMask, @NewDomainMask
			-- Get Next Key 
			FETCH NEXT FROM csr INTO @SpecimenCollectionUnitKey
		END -- end of while

		CLOSE csr

		DEALLOCATE csr
	END

	SET NOCOUNT OFF

GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('[usp_ConceptGroup_Update_ForLocalDomain]') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure [usp_ConceptGroup_Update_ForLocalDomain]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON [dbo].[usp_ConceptGroup_Update_ForLocalDomain] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_ConceptGroup_Update_ForLocalDomain] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[usp_ConceptGroup_Update_ForLocalDomain] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[usp_ConceptGroup_Update_ForLocalDomain] TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON [dbo].[usp_ConceptGroup_Update_ForLocalDomain] TO [Dev - JNCC SQL]
END

GO