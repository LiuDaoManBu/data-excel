package com.caotc.excel4j.matcher.usermodel;

import com.caotc.excel4j.matcher.BaseMatcher;
import com.caotc.excel4j.matcher.ComparableMatcher;
import com.caotc.excel4j.matcher.StringMatcher;
import com.caotc.excel4j.matcher.data.DataMatcher;
import com.caotc.excel4j.parse.result.StandardCell;

public class StandardCellMatcher extends BaseMatcher<StandardCell> {
  private DataMatcher valueMatcher;
  private ComparableMatcher<Integer> rowNumberMatcher;
  private ComparableMatcher<Integer> columnNumberMatcher;
  private StringMatcher columnStringMatcher;


}
