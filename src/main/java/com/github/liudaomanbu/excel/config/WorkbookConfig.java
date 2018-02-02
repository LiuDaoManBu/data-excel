package com.github.liudaomanbu.excel.config;

import java.util.Collection;
import java.util.Optional;
import org.apache.poi.ss.usermodel.Workbook;
import com.github.liudaomanbu.excel.parse.result.WorkbookParseResult;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;

public class WorkbookConfig extends Config<Workbook>{
  public static class Builder extends Config.Builder<Workbook>{
    private Collection<SheetConfig.Builder> sheetConfigBuilders;

    public Builder() {
      sheetConfigBuilders = Lists.newLinkedList();
    }

    public WorkbookConfig build() {
      return new WorkbookConfig(this);
    }

    public Collection<SheetConfig.Builder> getSheetConfigBuilders() {
      return sheetConfigBuilders;
    }

    public Builder setSheetConfigBuilders(Collection<SheetConfig.Builder> sheetConfigBuilders) {
      this.sheetConfigBuilders = sheetConfigBuilders;
      return this;
    }

  }

  public static Builder builder() {
    return new Builder();
  }

  private final ImmutableCollection<SheetConfig> sheetConfigs;

  private WorkbookConfig(Builder builder) {
    super(builder);
    sheetConfigs = builder.sheetConfigBuilders.stream()
        .peek(sheetConfigBuilder -> sheetConfigBuilder.setWorkbookConfig(this))
        .map(SheetConfig.Builder::build).collect(ImmutableSet.toImmutableSet());
  }

  public WorkbookParseResult parse(Workbook workbook) {
    return WorkbookParseResult.builder().setWorkbook(workbook).setConfig(this).build();
  }

  public ParserConfig getEffectiveParserConfig() {
    return Optional.ofNullable(getParserConfig()).orElse(ParserConfig.GLOBAL);
  }

  public ImmutableCollection<SheetConfig> getSheetConfigs() {
    return sheetConfigs;
  }
}
