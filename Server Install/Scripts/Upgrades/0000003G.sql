SET QUOTED_IDENTIFIER ON
GO

If EXISTS (
	SELECT * FROM SysObjects 
	WHERE Id = OBJECT_ID(N'[dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen]') 
	AND	  OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen]
GO

/*===========================================================================*\
  Description:	update the domain mask when user moving a local domain or 
				concept group results in the node falling inside a different domain branch, 
				then it is necessary to recalulate the entire domain structure of the 
				collection database for all relate date

  Parameters:
	@Collection_Unit_Key:	the unique key for collection unit
	@OldDomainMask:			the old domain mask 
	@NewDomainMask:			the new domain mask

 Created:	Qing Sun	28/11/2008

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] 
	@SpecimenCollectionUnitKey		Char(16),
	@OldDomainMask					INT,
	@NewDomainMask					INT		
AS

	SET NOCOUNT ON

	DECLARE	@ContainerKey			CHAR(16),
			@ParentCollectionKey	CHAR(16),
			@SpecimenMask			INT,
			@CurrentDomainMask		INT,
			@Collection_Key			CHAR(16)
		
		-- get current domain mask
	SELECT	@CurrentDomainMask		= Domain_Mask 
	FROM	Collection_Unit 
	WHERE	Collection_Unit_Key		= @SpecimenCollectionUnitKey	
	SET		@NewDomainMask			= @NewDomainMask	-	(@OldDomainMask & @NewDomainMask)
	SET		@OldDomainMask			= @OldDomainMask	-	(@OldDomainMask & @NewDomainMask)

	SET @SpecimenMask			= @CurrentDomainMask | @NewDomainMask  --> @SpecimenMask = SpecimenMask OR NewDomainMask

	IF @OldDomainMask <> 0
	BEGIN
		IF NOT EXISTS(
			SELECT		1 
			FROM		Collection_Unit 
			WHERE		Current_Container_Collection_Unit_Key			= @SpecimenCollectionUnitKey
			AND			Domain_Mask & @OldDomainMask					= @OldDomainMask
			UNION
			SELECT		1
			FROM		Specimen_Unit	S
			JOIN		Collection_Unit CU ON CU.Collection_Unit_Key	= S.Collection_Unit_Key
			WHERE		S.Parent_Collection_Collection_Unit_Key			= @SpecimenCollectionUnitKey
			AND			CU.Domain_Mask & @OldDomainMask					= @OldDomainMask
		)	
			SET @SpecimenMask = @SpecimenMask & ~ @OldDomainMask   ---> @SpecimenMask = SpecimenMask AND (NOT OldDomainMask)
		
	END
	IF  @SpecimenMask <> @CurrentDomainMask
	BEGIN
		UPDATE	Collection_Unit
		SET		Domain_Mask			= @SpecimenMask
		WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey
		
		SELECT	@ContainerKey			= CU.Current_Container_Collection_Unit_Key,
				@ParentCollectionKey	= SU.Parent_Collection_Collection_Unit_Key
		FROM	Collection_Unit CU
		JOIN	Specimen_Unit SU	ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
		WHERE	CU.Collection_Unit_Key	= @SpecimenCollectionUnitKey
		
		EXECUTE	[dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] @ContainerKey, @OldDomainMask,@NewDomainMask
		EXECUTE	[dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] @ParentCollectionKey, @OldDomainMask,@NewDomainMask
			/*-------------------------------------------------------------*\
		  And see if there was an impact on the domain masks stores in 
		  Conservation_Check and Conservation_Job.
		\*-------------------------------------------------------------*/
		EXECUTE [dbo].[usp_ConservationJob_Update_DomainMask]	@SpecimenCollectionUnitKey, NULL
		EXECUTE [dbo].[usp_ConservationCheck_Update_DomainMask] @SpecimenCollectionUnitKey, NULL
	END	
	SET NOCOUNT OFF

GO


IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('[dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen]') AND SysStat & 0xf = 4)
BEGIN
		PRINT 'Setting up security on procedure [usp_CollectionUnit_Update_DomainMask_ForSpecimen]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON [dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON [dbo].[usp_CollectionUnit_Update_DomainMask_ForSpecimen] TO [Dev - JNCC SQL]
END

GO

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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainKey_Get_ForLocalDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DomainKey_Get_ForLocalDomain]
GO

/*===========================================================================*\
  Description:	Outputs the concept group key for a concept.

  Parameters:	@Key	Local Domain key
  Return		@DomainKey  

  Created:	January 2008


\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DomainKey_Get_ForLocalDomain]
	@Key char(16),
	@DomainKey char(16) OUTPUT
AS
	SELECT 		@DomainKey = Domain_Key
	FROM		Local_Domain
	WHERE		Local_Domain_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainKey_Get_ForLocalDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure [usp_DomainKey_Get_ForLocalDomain]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON [dbo].[usp_DomainKey_Get_ForLocalDomain] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_DomainKey_Get_ForLocalDomain] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[usp_DomainKey_Get_ForLocalDomain] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON [dbo].[usp_DomainKey_Get_ForLocalDomain] TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[usp_DomainKey_Get_ForLocalDomain] TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON [dbo].[usp_DomainKey_Get_ForLocalDomain] TO [Dev - JNCC SQL]
END

GO


If EXISTS (
	SELECT * FROM SysObjects 
	WHERE Id = OBJECT_ID(N'[dbo].[usp_Domain_Update_ForSubjectArea]') 
	AND	  OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Domain_Update_ForSubjectArea]
GO

/*===========================================================================*\
  Description:	update the domain mask when user moving a Domain to different 
				Subject Area, then it is necessary to recalulate the entire 
				domain structure of the collection database for all relate date

  Parameters:
	@ParentKey	subject_Area_Key
	@ChildKey	Domain_Key
 

 Created:	Qing Sun	28/11/2008

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Domain_Update_ForSubjectArea] 
	@ParentKey	CHAR(16),
	@ChildKey	CHAR(16)
AS

	SET NOCOUNT ON

	UPDATE	Domain
	SET		Subject_Area_Key = @ParentKey
	WHERE	Domain_Key		 = @ChildKey

	SET NOCOUNT OFF

GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('[dbo].[usp_Domain_Update_ForSubjectArea]') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Domain_Update_ForSubjectArea'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON [dbo].[usp_Domain_Update_ForSubjectArea] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_Domain_Update_ForSubjectArea] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[usp_Domain_Update_ForSubjectArea] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[usp_Domain_Update_ForSubjectArea] TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON [dbo].[usp_Domain_Update_ForSubjectArea] TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocalDomainKey_Get_ForConceptGroup]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocalDomainKey_Get_ForConceptGroup]
GO

/*===========================================================================*\
  Description:	Outputs the concept group key for a concept.

  Parameters:	@Key	ConceptGroup key
  Return		@LocalDomainKey  

  Created:	January 2008


\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocalDomainKey_Get_ForConceptGroup]
	@Key char(16),
	@LocalDomainKey char(16) OUTPUT
AS
	SELECT 		@LocalDomainKey = Local_Domain_Key
	FROM		Concept_Group
	WHERE		Concept_Group_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocalDomainKey_Get_ForConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocalDomainKey_Get_ForConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON [dbo].[usp_LocalDomainKey_Get_ForConceptGroup] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_LocalDomainKey_Get_ForConceptGroup] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[usp_LocalDomainKey_Get_ForConceptGroup] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON [dbo].[usp_LocalDomainKey_Get_ForConceptGroup] TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[usp_LocalDomainKey_Get_ForConceptGroup] TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON [dbo].[usp_LocalDomainKey_Get_ForConceptGroup] TO [Dev - JNCC SQL]
END

GO

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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SubjectAreaKey_Get_ForDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SubjectAreaKey_Get_ForDomain]
GO

/*===========================================================================*\
  Description:	Outputs the concept group key for a concept.

  Parameters:	@Key	Domain key
  Return		@SubjectAreaKey  

  Created:	January 2008


\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SubjectAreaKey_Get_ForDomain]
	@Key char(16),
	@SubjectAreaKey char(16) OUTPUT
AS
	SELECT 		@SubjectAreaKey = Subject_Area_Key
	FROM		Domain
	WHERE		Domain_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SubjectAreaKey_Get_ForDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure [usp_SubjectAreaKey_Get_ForDomain]'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON [dbo].[usp_SubjectAreaKey_Get_ForDomain] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_SubjectAreaKey_Get_ForDomain] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[usp_SubjectAreaKey_Get_ForDomain] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON [dbo].[usp_SubjectAreaKey_Get_ForDomain] TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[usp_SubjectAreaKey_Get_ForDomain] TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON [dbo].[usp_SubjectAreaKey_Get_ForDomain] TO [Dev - JNCC SQL]
END

GO

