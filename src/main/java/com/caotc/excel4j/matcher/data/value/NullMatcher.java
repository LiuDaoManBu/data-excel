package com.caotc.excel4j.matcher.data.value;

public class NullMatcher extends DataValueMatcher {
	@Override
	public boolean support(Object value) {
		return Boolean.TRUE;
	}

	@Override
	public boolean matches(Object value) {
		return value!=null;
	}
}
