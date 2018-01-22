/*===========================================================================*\
  Description:	Procedure for creating 'Record Types' Concept Group.

  Created:	March 2004

  Last revision information:
    $Revision: 2 $
    $Date: 23/09/08 12:34 $
    $Author: Ericsalmon $

\*===========================================================================*/

IF NOT Exists(SELECT 1 FROM Concept_Group WHERE Concept_Group_Key = 'SYSTEM000000005T') 
	INSERT INTO Concept_Group (
		Concept_Group_Key, 
		Local_Domain_Key, 
		Item_Name, 
		Authority,
		Entered_Session_ID, 
		System_Supplied_Data
	) VALUES (
		'SYSTEM000000005T', 
		'SYSTEM0000000000', 
		'Record Types', 
		'System',
		'SYSTEM0000000000', 
		1
	)
GO