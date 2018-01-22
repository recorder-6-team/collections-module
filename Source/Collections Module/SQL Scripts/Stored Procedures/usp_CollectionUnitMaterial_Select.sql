/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitMaterial_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitMaterial_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of materials used for/with the specified 
		Collection Unit item

  Parameters:	@Key	Collection unit key

  Created:	September 2003

  Last revision information:
    $Revision: 4 $
    $Date: 31/03/04 10:07 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitMaterial_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Options to get performance benefits from VW_ConceptTerm.
	\*-------------------------------------------------------------*/
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	SELECT		CUM.Collection_Unit_Material_Key AS Item_Key,			
			CUM.Material_Concept_Key,
			CT1.Item_Name AS Material_Item_Name,
			CUM.Quantity,
			CUM.Unit_Concept_Key,
			CT2.Item_Name AS Unit_Item_Name,
			CUM.Custodian,
			CUM.[Timestamp]

	FROM		Collection_Unit_Material CUM
	INNER JOIN	vw_ConceptTerm CT1 ON CT1.Concept_Key = CUM.Material_Concept_Key
	LEFT JOIN	vw_ConceptTerm CT2 ON CT2.Concept_Key = CUM.Unit_Concept_Key

	WHERE		CUM.Collection_Unit_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitMaterial_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitMaterial_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Select TO [Dev - JNCC SQL]
END

GO