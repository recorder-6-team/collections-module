If EXISTS (
	SELECT * FROM SysObjects 
	WHERE Id = OBJECT_ID(N'[dbo].[usp_LocalDomain_Update_ForDomain]') 
	AND	  OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_LocalDomain_Update_ForDomain]
GO

/*===========================================================================*\
  Description:	update the domain mask when user moving a Local Domain to different 
				Domain, then it is necessary to recalulate the entire 
				domain structure of the collection database for all relate date

  Parameters:
	@ParentKey	Domain_Key
	@ChildKey	Local_Domain_Key
 

 Created:	Qing Sun	28/11/2008

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_LocalDomain_Update_ForDomain] 
	@ParentKey	CHAR(16),
	@ChildKey	CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE @OldDomainMask				INT,
			@NewDomainMask				INT,
			@SpecimenCollectionUnitKey	CHAR(16)

	-- get old domain mask
	SELECT	@OldDomainMask		=	D.Domain_Mask		
	FROM	Local_Domain	LD
	JOIN	Domain			D	ON	LD.Domain_Key = D.Domain_Key
	WHERE	LD.Local_Domain_Key	=	@ChildKey
			
	UPDATE	Local_Domain
	SET		Domain_Key			=	@ParentKey
	WHERE	Local_Domain_Key	=	@ChildKey

	-- get new domain mask
	SELECT	@NewDomainMask		=	D.Domain_Mask		
	FROM	Local_Domain	LD
	JOIN	Domain			D	ON	LD.Domain_Key = D.Domain_Key
	WHERE	LD.Local_Domain_Key	=	@ChildKey    

	IF @NewDomainMask <> @OldDomainMask
	BEGIN						
	-- Declare to find the distinct specimen domains
		DECLARE csr CURSOR FOR
			SELECT 	SU.Collection_Unit_Key		
			FROM 	Specimen_Unit	 SU
			JOIN	Determination	 D 	 WITH (INDEX (IX_Determination_Specimen)) 
										 ON	  D.Determination_Key	= SU.Preferred_Determination_Key
			JOIN	Concept			 C 	 WITH (INDEX (PK_Concept)) 
										 ON	  C.Concept_Key			= D.Concept_Key
			JOIN	Concept_Group	 CG  ON	  CG.Concept_Group_Key	= C.Concept_Group_Key
			JOIN	Local_Domain	 LD  ON	  LD.Local_Domain_Key	= CG.Local_Domain_Key
			JOIN	Domain			 DM  ON	  DM.Domain_Key			= LD.Domain_Key
										 AND  LD.Local_Domain_Key	= @ChildKey		
		OPEN csr
		-- get the first key
		FETCH NEXT FROM csr INTO @SpecimenCollectionUnitKey

		WHILE (@@FETCH_STATUS = 0)
		BEGIN
			EXECUTE	[dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] @SpecimenCollectionUnitKey, @OldDomainMask, @NewDomainMask
			-- Get next key
			FETCH NEXT FROM csr INTO @SpecimenCollectionUnitKey
		END
		
		CLOSE csr

		DEALLOCATE csr

	END

	SET NOCOUNT OFF

GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('[dbo].[usp_LocalDomain_Update_ForDomain]') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure [usp_LocalDomain_Update_ForDomain]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON [dbo].[usp_LocalDomain_Update_ForDomain] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_LocalDomain_Update_ForDomain] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[usp_LocalDomain_Update_ForDomain] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[usp_LocalDomain_Update_ForDomain] TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON [dbo].[usp_LocalDomain_Update_ForDomain] TO [Dev - JNCC SQL]
END

GO