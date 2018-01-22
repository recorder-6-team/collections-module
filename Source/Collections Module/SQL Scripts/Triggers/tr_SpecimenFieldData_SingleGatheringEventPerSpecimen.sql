/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[tr_SpecimenFieldData_SingleGatheringEventPerSpecimen]') 
	   AND    Type = 'TR')
    DROP TRIGGER [dbo].[tr_SpecimenFieldData_SingleGatheringEventPerSpecimen]
GO

/*===========================================================================*\
  Description:	Update Gathering_Event so that Specimens can have no more than one
				gathering event.

  Type:		AFTER INSERT, UPDATE

  Created:	October 2003

  Last revision information:
    $Revision: 2 $
    $Date: 7-03-06 13:38 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE TRIGGER [tr_SpecimenFieldData_SingleGatheringEventPerSpecimen] ON [dbo].[SPECIMEN_FIELD_DATA] 
FOR INSERT, UPDATE
AS

DECLARE @CurrentSFDKey CHAR(16)
DECLARE @GatheringEvent BIT
DECLARE @CurrentSUKey CHAR(16)

SELECT @CurrentSFDKey = Specimen_Field_Data_Key, 
	@GatheringEvent = Gathering_Event, 
	@CurrentSUKey = Collection_Unit_Key 
FROM INSERTED

IF @GatheringEvent = 1
	UPDATE SPECIMEN_FIELD_DATA
	SET Gathering_Event = 0
	WHERE Collection_Unit_Key = @CurrentSUKey
		AND Specimen_Field_Data_Key <> @CurrentSFDKey
GO