/*===========================================================================*\
  Description:	Deletes all Quick Entry Sessions which have been successfully
				validated and processed.  This is a one off script to clean
				up the database following errors deleting sessions.  This should
				only be run when no clients are connected to the Collections 
				module. 

  Parameters:	

  Created:	June 2016

  Last revision information:
    $Revision: 1 $
    $Date: 21/06/16 13:11 $
    $Author: Christopherknight $

\*===========================================================================*/

DECLARE @DeletableSessions TABLE(QE_Session_Key INT)

INSERT INTO @DeletableSessions
SELECT QE_Session_Key
FROM QE_Session
WHERE QE_Session_Key NOT IN( 
	SELECT DISTINCT(S.QE_Session_Key)
	FROM QE_Session S
	INNER JOIN QE_Data_Row DR
		ON S.QE_Session_Key = DR.QE_Session_Key
	WHERE DR.General = 0 
		AND (
			DR.Validated = 0
			OR DR.Processed = 0	
			)
	) 

DELETE DI
FROM QE_Data_Item DI
INNER JOIN QE_Data_Row DR
	ON DI.QE_Data_Row_Key = DR.QE_Data_Row_Key
INNER JOIN @DeletableSessions DS
	ON DR.QE_Session_Key = DS.QE_Session_Key 

DELETE DR 
FROM QE_Data_Row DR
INNER JOIN @DeletableSessions DS
	ON DR.QE_Session_Key = DS.QE_Session_Key

DELETE S 
FROM QE_Session S
INNER JOIN @DeletableSessions DS
	ON S.QE_Session_Key = DS.QE_Session_Key