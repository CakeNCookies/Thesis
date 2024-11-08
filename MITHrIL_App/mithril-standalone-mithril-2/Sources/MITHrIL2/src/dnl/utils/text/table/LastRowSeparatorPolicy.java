package dnl.utils.text.table;

import javax.swing.table.TableModel;

public class LastRowSeparatorPolicy extends SeparatorPolicy {

	public LastRowSeparatorPolicy() {
	}

	public LastRowSeparatorPolicy(TableModel tableModel) {
		super(tableModel);
	}

	@Override
	public boolean hasSeparatorAt(int row) {
		return row == tableModel.getRowCount() - 1;
	}

}
