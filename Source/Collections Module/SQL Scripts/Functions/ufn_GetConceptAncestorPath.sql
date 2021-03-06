/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.ufn_GetConceptAncestorPath')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION dbo.ufn_GetConceptAncestorPath
GO

/*===========================================================================*\
  Description:	Returns the hierarchy of a specified concept

  Parameters:	@Concept_Key		The key of the concept for which the ancestor
									hierarchy is return

  Created:	May 2011

  Last revision information:
    $Revision: 5 $
    $Date: 31/08/11 10:52 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_GetConceptAncestorPath (
	@ItemName varchar(100),
	@Concept_Key char(16)
)
RETURNS varchar(5000)
AS
BEGIN
	DECLARE @lineageId INT,
			@HierarchyPath varchar(5000),
			@CurrentLineage varchar(1000),
			@current_ancestor varchar(200)

	SET @HierarchyPath = ''
	
	DECLARE conceptLineageId CURSOR LOCAL FAST_FORWARD FOR
	SELECT lineage_id 
	FROM concept_lineage 
	WHERE concept_key = @Concept_Key

	OPEN conceptLineageId

	WHILE 1 = 1
	BEGIN

		FETCH conceptLineageId
		INTO @lineageId

		IF @@FETCH_STATUS <> 0 BREAK

		SET @CurrentLineage = ''

		DECLARE		ancestors	CURSOR LOCAL FAST_FORWARD FOR
		select crelated.published_term 
		from concept c 
		left join concept_lineage cl on cl.concept_key = c.concept_key
		inner join (
			select cl1.lineage, cl1.lineage_id, c.concept_group_key, c.published_term
			from concept_lineage cl1
			inner join concept c on c.concept_key = cl1.concept_key ) as crelated
			on cl.lineage LIKE crelated.lineage + '\%' 
				and c.concept_group_key = crelated.concept_group_key
		where c.concept_key = @Concept_Key and cl.lineage_id = @lineageId
		order by crelated.lineage

		OPEN		ancestors

		WHILE 1 = 1
		BEGIN
			FETCH		ancestors
			INTO		@current_ancestor

			IF @@FETCH_STATUS <> 0 BREAK
	
			IF LEN(@CurrentLineage) = 0
			BEGIN
				SET @CurrentLineage = @ItemName + ': '
			END

			SET @CurrentLineage = @CurrentLineage + @current_ancestor + ' - '
		END
	
		CLOSE		ancestors
		DEALLOCATE	ancestors

		IF LEN(@CurrentLineage) > 0 
		BEGIN
			SELECT @CurrentLineage = SUBSTRING(@CurrentLineage, 0, LEN(@CurrentLineage) - 1)								 
		END ELSE
		BEGIN
			SELECT @CurrentLineage = @ItemName	
		END

		SET @HierarchyPath = @HierarchyPath + @CurrentLineage + '**'
	END
	
	CLOSE conceptLineageId
	DEALLOCATE conceptLineageId

	IF LEN(@HierarchyPath) > 0
	BEGIN
		SET @HierarchyPath = SUBSTRING(@HierarchyPath, 0, LEN(@HierarchyPath) - 1)
	END
	ELSE
	BEGIN
		SET @HierarchyPath = @ItemName
	END

	RETURN @HierarchyPath
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.ufn_GetConceptAncestorPath')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function dbo.ufn_GetConceptAncestorPath'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [Dev - JNCC SQL]
	END
GO