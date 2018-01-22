If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ReportBlockOrders_Select]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ReportBlockOrders_Select]
GO

CREATE PROCEDURE [dbo].[usp_ReportBlockOrders_Select] 
@ReportBlockKey CHAR(16)

AS

--  DESCRIPTION
--  Returns the allowed ordering that can be applied to a Report Block
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@ReportBlockKey		Report Block which ordering is to be found
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2004-05-01
--
SET NOCOUNT ON

SELECT Report_Block_Order_Key, Item_Name, Order_Clause_SQL
FROM
	Report_Block_Order
WHERE Report_Block_Key = @ReportBlockKey
ORDER BY Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReportBlockOrders_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReportBlockOrders_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [Dev - JNCC SQL]
END

GO
