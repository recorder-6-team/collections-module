/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonList_Select]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonList_Select]
GO

/*===========================================================================*\
  Description:	Select all taxon lists.

  Parameters:

  Created:		Jan 2004

  Last revision information:
	$Revision: 1 $
	$Date: 23/06/11 17:03 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonList_Select]
AS
	SET NOCOUNT ON

	SELECT		TAXON_LIST_KEY,
				ITEM_NAME
	FROM		TAXON_LIST
	ORDER BY	ITEM_NAME
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonList_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonList_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonList_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonList_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonList_Select TO [Dev - JNCC SQL]
END
GO