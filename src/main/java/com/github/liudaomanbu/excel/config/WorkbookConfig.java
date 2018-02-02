package com.github.liudaomanbu.excel.config;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import org.apache.poi.ss.usermodel.Workbook;
import com.github.liudaomanbu.excel.parse.result.WorkbookParseResult;
import com.github.liudaomanbu.excel.validator.Validator;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;

public class WorkbookConfig extends Config{
  public static class Builder extends Config.Builder{
    private Collection<SheetConfig.Builder> sheetConfigBuilders;
    private List<Validator<Workbook>> validators;

    public Builder() {
      sheetConfigBuilders = Lists.newLinkedList();
      validators = Lists.newLinkedList();
    }

    public WorkbookConfig build() {
      return new WorkbookConfig(this);
    }

    public List<Validator<Workbook>> getValidators() {
      return validators;
    }

    public Builder setValidators(List<Validator<Workbook>> validators) {
      this.validators = validators;
      return this;
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
  private final ImmutableList<Validator<Workbook>> validators;

  private WorkbookConfig(Builder builder) {
    super(builder);
    sheetConfigs = builder.sheetConfigBuilders.stream()
        .peek(sheetConfigBuilder -> sheetConfigBuilder.setWorkbookConfig(this))
        .map(SheetConfig.Builder::build).collect(ImmutableSet.toImmutableSet());
    validators = builder.validators.stream().collect(ImmutableList.toImmutableList());
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

  public ImmutableList<Validator<Workbook>> getValidators() {
    return validators;
  }

}
