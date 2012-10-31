	$(function() {
		function addRow(e) {
			// fetch the row (which is the parent of the add button)
			var $row = $(this).parent();
			// clone the row
			var $newRow = $row.clone();
			// clear the content of the new row
			clearRow($newRow);
			// insert the new row after the current one
			$newRow.insertAfter($row);
			// fetch all the row of the same class
			var $rows = $('.' + $row.attr('class'));
			// update all the row indices
			updateRowIndices($rows);
			//block any other action by the <a> tag
			e.preventDefault();
			return true;
		}

		function removeRow(e) {
			// fetch the row (which is the parent of the add button)
			var $row = $(this).parent();
			// fetch all the row of the same class
			var $rows = $('.' + $row.attr('class'));

			//Allow row deletion if not the only row
			if ($rows.length > 1) {
				$(this).parent().remove();
			}
			// refetch all the rows and update indices
			var $rows = $('.' + $row.attr('class'));
			updateRowIndices($rows);
			//block any other action by the <a> tag
			e.preventDefault();
			return true;
		}

		function updateRowIndices($rows) {
			// iterate through all the rows
			$rows.each(function(i, row) {
				// fetch all the form elements
				var $inputs = $(row).children('input,select');
				// iterate through all the form elements
				$inputs.each(function(j, input) {
					// get the 'name attribut'
					var $name_attr = $(input).attr('name');
					// check whether is has an index
					var $matches = $name_attr.match('\^(\\S+)\\[\\d+\\]\$');
					// in such a case...
					if ($matches) {
						// ...get the prefix (i.e. the first group matched in the regex)
						var $prefix = $matches[1];
						// update the name with the new index
						$(input).attr('name', $prefix + '[' + i + ']' );
					}
				});
			});
		}

		function clearRow (row) {
			// clear the val attribute of childrene element of the row
			$(row).children('*').each(function(i,e) {
				$(e).val('');
			});
		}

		// attach the add and remove functions to the add and remove buttons
		$('.add-button').live("click", addRow);
		$('.remove-button').live("click", removeRow);
	});