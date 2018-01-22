-- First, we need to cleverly remove duplicate data items in quick entry
DECLARE @RowKey CHAR(16)
DECLARE @FieldKey CHAR(16)
DECLARE @Repeats INT

DECLARE csr CURSOR FOR
	SELECT QE_Data_Row_Key, QE_Template_Field_Key, Count(*) AS Repeats
	FROM QE_Data_Item
	GROUP BY QE_Data_Row_Key, QE_Template_Field_Key
	HAVING Count(*)>1

OPEN csr

WHILE 1=1
BEGIN
	FETCH NEXT FROM csr INTO @RowKey, @FieldKey, @Repeats
	IF @@FETCH_STATUS<>0
		BREAK

	-- Make sure the delete statement deletes all but the last data item.
	SET @Repeats = @Repeats-1
	SET ROWCOUNT @Repeats

	DELETE FROM QE_Data_Item 
	WHERE QE_Data_Item_Key IN (
		SELECT QE_Data_Item_Key
		FROM QE_Data_Item
		WHERE QE_Data_Row_Key=@RowKey
		AND QE_Template_Field_Key=@FieldKey
	)
END

CLOSE csr
DEALLOCATE csr

SET ROWCOUNT 0

IF NOT EXISTS(SELECT 1 FROM sysconstraints WHERE [constid]=Object_ID('IX_QE_Data_Item_Unique'))
	ALTER TABLE [dbo].[QE_Data_Item] ADD 
		CONSTRAINT [IX_QE_Data_Item_Unique] UNIQUE  NONCLUSTERED 
		(
			[QE_Data_Row_Key],
			[QE_Template_Field_Key]
		)  ON [PRIMARY] 
	GO