package com.caotc.excel4j.constant;

import org.apache.poi.ss.usermodel.Cell;

import com.caotc.excel4j.parse.result.StandardCell;
import com.caotc.excel4j.util.ExcelUtil;

public enum Direction {
	TOP {
		@Override
		public Direction getNegativeDirection() {
			return BOTTOM;
		}

		@Override
		Index getIndex(StandardCell cell) {
			return new Index(cell.getFirstRow(),cell.getValueCell().getColumnIndex());
		}
		
		@Override
		Index getTargetIndex(Index index) {
			return new Index(--index.rowIndex,index.columnIndex);
		}
	},
	BOTTOM {
		@Override
		public Direction getNegativeDirection() {
			return TOP;
		}

		@Override
		Index getIndex(StandardCell cell) {
			return new Index(cell.getLastRow(),cell.getValueCell().getColumnIndex());
		}
		
		@Override
		Index getTargetIndex(Index index) {
			return new Index(++index.rowIndex,index.columnIndex);
		}
	},
	LEFT {
		@Override
		public Direction getNegativeDirection() {
			return RIGHT;
		}

		@Override
		Index getIndex(StandardCell cell) {
			return new Index(cell.getValueCell().getRowIndex(),cell.getFirstColumn());
		}
		
		@Override
		Index getTargetIndex(Index index) {
			return new Index(index.rowIndex,--index.columnIndex);
		}
	},
	RIGHT {
		@Override
		public Direction getNegativeDirection() {
			return LEFT;
		}

		@Override
		Index getIndex(StandardCell cell) {
			return new Index(cell.getValueCell().getRowIndex(),cell.getLastColumn());
		}
		
		@Override
		Index getTargetIndex(Index index) {
			return new Index(index.rowIndex,++index.columnIndex);
		}
	};
	public abstract Direction getNegativeDirection();
	
	public StandardCell nextCell(StandardCell cell){
		Index index=getTargetIndex(getIndex(cell));
		return ExcelUtil.getStandardCellByIndex(cell.getSheet(), index.rowIndex, index.columnIndex);
	}
	
//	public List<Cell> nextCells(Cell cell,boolean mergedRegionFlag){
//		Index index=getTargetIndex(getIndex(cell, mergedRegionFlag));
//		return getFirstCell(ExcelUtil.getCellByIndex(cell.getSheet(), index.rowIndex, index.columnIndex)
//				,mergedRegionFlag);
//	}
	
	public StandardCell getCell(StandardCell cell, int distance){
		for(int i=0;i<distance;i++){
			cell=nextCell(cell);
		}
		return cell;
	}
	
	abstract Index getIndex(StandardCell cell);
	abstract Index getTargetIndex(Index index);
	
	private static class Index{
		private Integer rowIndex;
		private Integer columnIndex;
		public Index(Integer rowIndex, Integer columnIndex) {
			super();
			this.rowIndex = rowIndex;
			this.columnIndex = columnIndex;
		}
	}
}