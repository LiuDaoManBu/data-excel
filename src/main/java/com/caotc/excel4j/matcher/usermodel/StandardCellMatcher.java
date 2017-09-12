package com.caotc.excel4j.matcher.usermodel;

import java.util.Collection;

import com.caotc.excel4j.matcher.data.DataMatcher;
import com.caotc.excel4j.matcher.data.value.ComparableValueMatcher;
import com.caotc.excel4j.matcher.data.value.StringMatcher;
import com.caotc.excel4j.parse.result.StandardCell;
import com.caotc.excel4j.util.ExcelUtil;
import com.caotc.excel4j.util.MatcherUtil;

public class StandardCellMatcher{
	private DataMatcher valueMatcher;
	private Collection<ComparableValueMatcher<Integer>> rowNumberMatchers;
	private Collection<ComparableValueMatcher<Integer>> columnNumberMatchers;
	private Collection<StringMatcher> columnStringMatchers;
	
	public boolean matches(StandardCell cell){
		return valueMatcher.matches(cell.getValue()) && MatcherUtil.allMatches(rowNumberMatchers
				, cell.getValueCell().getRowIndex()) && MatcherUtil.allMatches(columnNumberMatchers, cell.getValueCell().getColumnIndex()) 
				&& MatcherUtil.allMatches(columnStringMatchers, ExcelUtil.indexToColumn(cell.getValueCell().getColumnIndex()));
	}
}
