package com.caotc.util.excel.matcher.data;

import java.util.Collection;

import com.caotc.util.excel.MatcherUtil;
import com.caotc.util.excel.matcher.Matcher;
import com.caotc.util.excel.matcher.data.type.DataType;
import com.caotc.util.excel.matcher.data.value.DataValueMatcher;

public class DataMatcher implements Matcher {
	private Collection<DataType> dataTypes;
	private Collection<Collection<DataValueMatcher>> dataValueMatchers;
	private Collection<Collection<? extends Matcher>> matchers; 
	
	@Override
	public boolean support(Object value) {
		return matchers.stream().allMatch(matcher-> MatcherUtil.allSupport(matcher, value));
	}

	@Override
	public boolean matches(Object value) {
		return MatcherUtil.anyMatches(matchers, value);
	}
}
