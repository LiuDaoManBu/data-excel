package com.caotc.excel4j.config;

import java.util.Objects;
import com.caotc.excel4j.constant.ConstructType;
import com.google.common.base.Preconditions;

public class TableDataConfig<V> extends DataConfig<V> {

  public static class Builder<V> extends DataConfig.Builder<V> {
    private TableConfig tableConfig;
    private ConstructType constructType;

    public TableDataConfig<V> build() {
      // TODO 提示语
      Preconditions.checkState(Objects.nonNull(tableConfig));
      return new TableDataConfig<V>(this);
    }

    public TableConfig getTableConfig() {
      return tableConfig;
    }

    public Builder<V> setTableConfig(TableConfig tableConfig) {
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

  }

  private final TableConfig tableConfig;
  private final ConstructType constructType;

  protected TableDataConfig(Builder<V> builder) {
    super(builder);
    this.constructType = builder.constructType;
    this.tableConfig = builder.tableConfig;
  }

  public ConstructType getConstructType() {
    return constructType;
  }

  public TableConfig getTableConfig() {
    return tableConfig;
  }
  
}
