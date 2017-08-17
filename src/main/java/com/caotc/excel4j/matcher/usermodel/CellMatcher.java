package com.caotc.excel4j.matcher.usermodel;

import java.util.Collection;

import org.apache.poi.ss.usermodel.Cell;

import com.caotc.excel4j.matcher.data.DataMatcher;
import com.caotc.excel4j.matcher.data.value.ComparableValueMatcher;
import com.caotc.excel4j.matcher.data.value.StringMatcher;
import com.caotc.excel4j.util.ExcelUtil;
import com.caotc.excel4j.util.MatcherUtil;

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
