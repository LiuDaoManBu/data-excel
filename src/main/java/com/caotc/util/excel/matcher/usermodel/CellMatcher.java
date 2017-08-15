package com.caotc.util.excel.matcher.usermodel;

import java.util.Collection;

import org.apache.poi.ss.usermodel.Cell;

import com.caotc.util.excel.ExcelUtil;
import com.caotc.util.excel.MatcherUtil;
import com.caotc.util.excel.matcher.data.DataMatcher;
import com.caotc.util.excel.matcher.data.value.ComparableValueMatcher;
import com.caotc.util.excel.matcher.data.value.StringMatcher;

public class CellMatcher{
	private DataMatcher valueMatcher;
	private Collection<ComparableValueMatcher<Integer>> rowNumberMatchers;
	private Collection<ComparableValueMatcher<Integer>> columnNumberMatchers;
	private Collection<StringMatcher> columnStringMatchers;
	
	public boolean matches(Cell cell){
		return valueMatcher.matches(ExcelUtil.getValue(cell)) && MatcherUtil.allMatches(rowNumberMatchers
				, cell.getRowIndex()) && MatcherUtil.allMatches(columnNumberMatchers, cell.getColumnIndex()) 
				&& MatcherUtil.allMatches(columnStringMatchers, ExcelUtil.indexToColumn(cell.getColumnIndex()));
	}
}
