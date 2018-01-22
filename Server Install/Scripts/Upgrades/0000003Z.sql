IF EXISTS (select * from sysindexes
				where id=object_id('Collection_Unit_Number') and name='IX_Collection_Unit_Number_Number')
DROP INDEX IX_Collection_Unit_Number_Number ON Collection_Unit_Number
GO

/*=================================================================================*\
	
	Description:	Adds an index to the field 'Number' in 'Collection_Unit_Number'
					table to allow more efficient searches.

	Created:		September 2010

\*=================================================================================*/

CREATE NONCLUSTERED INDEX [IX_Collection_Unit_Number_Number] ON [dbo].[Collection_Unit_Number] 
(
	Number ASC
)
GO