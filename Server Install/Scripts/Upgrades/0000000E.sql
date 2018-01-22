/*===========================================================================*\
  Description:	Insert barcode value into Setting table.

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 27/08/04 11:38 $
    $Author: Johnvanbreda $

\*===========================================================================*/
IF NOT EXISTS (Select 1 FROM Setting WHERE [Name]='Barcode')
	INSERT INTO Setting (
		[Name],
		Data
	) VALUES (
		'Barcode',
		5
	)
