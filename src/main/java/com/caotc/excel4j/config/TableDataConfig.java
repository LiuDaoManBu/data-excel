package com.caotc.excel4j.config;

import java.util.Map;
import java.util.Objects;
import com.caotc.excel4j.constant.ConstructType;
import com.caotc.excel4j.matcher.Matcher;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.base.Preconditions;

public class TableDataConfig<V> extends DataConfig<V> {

  public static class Builder<V> extends DataConfig.Builder<V> {
    private TableConfig<V> tableConfig;
    private ConstructType constructType;
    private Matcher.Builder<Map<Menu<?>, StandardCell>> matcherBuilder;

    public TableDataConfig<V> build() {
      // TODO 提示语
      Preconditions.checkState(Objects.nonNull(tableConfig));
      return new TableDataConfig<>(this);
    }

    public TableConfig<V> getTableConfig() {
      return tableConfig;
    }

    public Builder<V> setTableConfig(TableConfig<V> tableConfig) {
      this.tableConfig = tableConfig;
      return this;
    }

    public ConstructType getConstructType() {
      return constructType;
    }

    public Builder<V> setConstructType(ConstructType constructType) {
      this.constructType = constructType;
      return this;
    }

    public Matcher.Builder<Map<Menu<?>, StandardCell>> getMatcherBuilder() {
      return matcherBuilder;
    }

    public Builder<V> setMatcherBuilder(Matcher.Builder<Map<Menu<?>, StandardCell>> matcherBuilder) {
      this.matcherBuilder = matcherBuilder;
      return this;
    }

  }

  private final TableConfig<V> tableConfig;
  private final ConstructType constructType;
  private final Matcher<Map<Menu<?>, StandardCell>> matcher;

  protected TableDataConfig(Builder<V> builder) {
    super(builder);
    this.constructType = builder.constructType;
    this.tableConfig = builder.tableConfig;
    this.matcher=builder.matcherBuilder.build();
  }

  public ConstructType getConstructType() {
    return constructType;
  }

  public TableConfig<V> getTableConfig() {
    return tableConfig;
  }

  public Matcher<Map<Menu<?>, StandardCell>> getMatcher() {
    return matcher;
  }
  
}
