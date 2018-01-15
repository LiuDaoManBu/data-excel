package com.caotc.excel4j.matcher.usermodel;

import com.caotc.excel4j.matcher.BaseMatcher;
import com.caotc.excel4j.parse.result.Table;

public class TableMatcher extends BaseMatcher<Table> {
  public static class Builder extends BaseMatcher.Builder<Table> {

    @Override
    public TableMatcher build() {
      return new TableMatcher(this);
    }
    
  }

  public static Builder builder() {
    return new Builder();
  }
  
  private TableMatcher(Builder builder) {
    super(builder);
  }
}