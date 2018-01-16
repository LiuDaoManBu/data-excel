package com.caotc.excel4j.config;

import java.util.Objects;
import com.caotc.excel4j.constant.ConstructType;
import com.google.common.base.Preconditions;

public class TableDataConfig<V> extends DataConfig<V> {

  public static class Builder<V> extends DataConfig.Builder<V> {
    private TableConfig<V> tableConfig;
    private ConstructType constructType;

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

  }

  private final TableConfig<V> tableConfig;
  private final ConstructType constructType;

  protected TableDataConfig(Builder<V> builder) {
    super(builder);
    this.constructType = builder.constructType;
    this.tableConfig = builder.tableConfig;
  }

  public ConstructType getConstructType() {
    return constructType;
  }

  public TableConfig<V> getTableConfig() {
    return tableConfig;
  }
  
}
