/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[VW_CollectionUnitAccessionNumber]') 
	   AND    ObjectProperty(Id, N'IsView') = 1)
    DROP VIEW [dbo].[VW_CollectionUnitAccessionNumber]
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	View that lists collection units and their accession numbers

  Created:	August 2004

  Last revision information:
    $Revision: 1 $
    $Date: 24/08/04 12:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_CollectionUnitAccessionNumber]
AS

SELECT	
		CU.Collection_Unit_Key, 
		M.Number AS Accession_Number
FROM		Collection_Unit CU
LEFT JOIN 	(Movement_Collection_Unit MCU 
INNER JOIN 	Movement_Direction MD 
		ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
		AND MD.OutBound = 0			-- Inbound = 0
INNER JOIN 	Movement M 
		ON M.Movement_Key = MD.Movement_Key
		AND		M.Movement_Type IN (0, 1))
ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key

GO

SET NUMERIC_ROUNDABORT OFF 
SET ARITHABORT  OFF 
GO

SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT SELECT ON [dbo].[VW_CollectionUnitAccessionNumber] TO [Public]
GO