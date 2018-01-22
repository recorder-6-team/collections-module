If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_RefreshDomainMask]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_RefreshDomainMask]
GO

/*===========================================================================*\
  Description:	Returns Collections associated with a specified Condition Check.
	This is a support stored procedure for system administrators rather than
	a stored proc accessed from within the application.

  Parameters:	
	None

  Created:	April 2008

  Last revision information:
    $Revision: 1 $
    $Date: 18/04/08 12:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Collections_RefreshDomainMask] 
AS

-- Reset the domain masks for all collections
UPDATE CU
SET CU.Domain_Mask=0
FROM Collection_Unit CU
INNER JOIN Collection C ON C.Collection_Unit_Key=CU.Collection_Unit_Key

DECLARE @CollectionUnitKey CHAR(16), @DomainMask INT

-- Declare to find the distinct specimen domains
DECLARE csr CURSOR FOR
	SELECT DISTINCT CUCol.Collection_Unit_Key, CUSpec.Domain_Mask
	FROM Collection_Unit CUCol
	INNER JOIN Collection CCol ON CCol.Collection_Unit_Key=CUCol.Collection_Unit_Key
	INNER JOIN Specimen_Unit SU ON SU.Parent_Collection_Collection_Unit_Key=CUCol.Collection_Unit_Key
	INNER JOIN Collection_Unit CUSpec ON CUSpec.Collection_Unit_Key=SU.Collection_Unit_Key

OPEN csr

-- OR each domain into the Collection. Note a SUM won't work in the case that a specimen has 2 domains.
WHILE (1=1)
BEGIN
	FETCH NEXT FROM csr INTO @CollectionUnitKey, @DomainMask
	IF @@FETCH_STATUS<>0 BREAK

	UPDATE Collection_Unit SET Domain_Mask = Domain_Mask | @DomainMask
	WHERE Collection_Unit_Key=@CollectionUnitKey

END

CLOSE csr

DEALLOCATE csr

GO
