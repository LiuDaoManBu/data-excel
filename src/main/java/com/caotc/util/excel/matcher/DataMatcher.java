package com.caotc.util.excel.matcher;

import java.util.Collection;

import com.caotc.util.excel.MatcherUtil;

public class DataMatcher implements Matcher {
	private Collection<DataType> dataTypes;
	private Collection<DataValueMatcher> dataValueMatchers;
	private Collection<Matcher> matchers;
	
	@Override
	public boolean support(Object value) {
		return MatcherUtil.allSupport(matchers, value);
	}

	@Override
	public boolean matches(Object value) {
		return MatcherUtil.allMatches(matchers, value);
	}
}
