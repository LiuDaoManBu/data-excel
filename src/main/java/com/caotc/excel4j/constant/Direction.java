package com.caotc.excel4j.constant;

import org.apache.poi.ss.usermodel.Cell;

import com.caotc.excel4j.util.ExcelUtil;

public enum Direction {
	TOP {
		@Override
		public Direction getNegativeDirection() {
			return BOTTOM;
		}

		@Override
		Index getMergedRegionIndex(Cell cell) {
			return new Index(ExcelUtil.getMergedRegion(cell).getFirstRow(),cell.getColumnIndex());
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
		Index getMergedRegionIndex(Cell cell) {
			return new Index(ExcelUtil.getMergedRegion(cell).getLastRow(),cell.getColumnIndex());
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
		Index getMergedRegionIndex(Cell cell) {
			return new Index(cell.getRowIndex(),ExcelUtil.getMergedRegion(cell).getFirstColumn());
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
		Index getMergedRegionIndex(Cell cell) {
			return new Index(cell.getRowIndex(),ExcelUtil.getMergedRegion(cell).getLastColumn());
		}
		
		@Override
		Index getTargetIndex(Index index) {
			return new Index(index.rowIndex,++index.columnIndex);
		}
	};
	private static final boolean DEFAULT_MERGED_REGION_FLAG=Boolean.TRUE;
	public abstract Direction getNegativeDirection();
	private Index getCellIndex(Cell cell){
		return new Index(cell.getRowIndex(),cell.getColumnIndex());
	}
	abstract Index getMergedRegionIndex(Cell cell);
	Index getIndex(Cell cell,boolean mergedRegionFlag){
		if(mergedRegionFlag && ExcelUtil.isMergedRegion(cell)){
			return getMergedRegionIndex(cell);
		}else{
			return getCellIndex(cell);
		}
	}
	abstract Index getTargetIndex(Index index);
	Cell getFirstCell(Cell cell,boolean mergedRegionFlag){
		if(mergedRegionFlag){
			cell = ExcelUtil.getFirstCell(cell);
		}
		return cell;
	}
	public Cell nextCell(Cell cell,boolean mergedRegionFlag){
		Index index=getTargetIndex(getIndex(cell, mergedRegionFlag));
		return getFirstCell(ExcelUtil.getCellByIndex(cell.getSheet(), index.rowIndex, index.columnIndex)
				,mergedRegionFlag);
	}
	public Cell nextCell(Cell cell){
		return nextCell(cell,DEFAULT_MERGED_REGION_FLAG);
	}
	public Cell getCell(Cell cell, int distance,boolean mergedRegionFlag){
		for(int i=0;i<distance;i++){
			cell=nextCell(cell,mergedRegionFlag);
		}
		return cell;
	}
	public Cell getCell(Cell cell, int distance){
		return getCell(cell,distance,DEFAULT_MERGED_REGION_FLAG);
	}
	
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