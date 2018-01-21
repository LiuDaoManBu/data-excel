package com.github.liudaomanbu.excel.config;

import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import org.apache.poi.ss.usermodel.Sheet;
import com.github.liudaomanbu.excel.constant.Direction;
import com.github.liudaomanbu.excel.parse.result.Table;
import com.github.liudaomanbu.excel.validator.Validator;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import com.google.common.collect.Streams;
import com.google.common.graph.SuccessorsFunction;
import com.google.common.graph.Traverser;

// TODO classType?
public class TableConfig extends Config {
  public static class Builder extends Config.Builder {
    private List<MenuConfig.Builder> topMenuConfigBuilders;
    private SheetConfig sheetConfig;
    private Direction menuDirection;
    private TableDataConfig.Builder dataConfigBuilder;
    private List<Validator<Table>> validators;
    private ParserConfig parserConfig;

    public Builder() {
      topMenuConfigBuilders = Lists.newLinkedList();
      validators = Lists.newLinkedList();
    }

    public TableConfig build() {
      return new TableConfig(this);
    }

    @Override
    public Builder setId(Object id) {
      super.setId(id);
      return this;
    }
    
    public SheetConfig getSheetConfig() {
      return sheetConfig;
    }

    public Builder setSheetConfig(SheetConfig sheetConfig) {
      this.sheetConfig = sheetConfig;
      return this;
    }

    public Direction getMenuDirection() {
      return menuDirection;
    }

    public Builder setMenuDirection(Direction menuDirection) {
      this.menuDirection = menuDirection;
      return this;
    }

    public List<MenuConfig.Builder> getTopMenuConfigBuilders() {
      return topMenuConfigBuilders;
    }

    public Builder setTopMenuConfigBuilders(List<MenuConfig.Builder> topMenuConfigBuilders) {
      this.topMenuConfigBuilders = topMenuConfigBuilders;
      return this;
    }

    public ParserConfig getParserConfig() {
      return parserConfig;
    }

    public Builder setParserConfig(ParserConfig parserConfig) {
      this.parserConfig = parserConfig;
      return this;
    }

    public TableDataConfig.Builder getDataConfigBuilder() {
      return dataConfigBuilder;
    }

    public Builder setDataConfigBuilder(TableDataConfig.Builder dataConfigBuilder) {
      this.dataConfigBuilder = dataConfigBuilder;
      return this;
    }

    public List<Validator<Table>> getValidators() {
      return validators;
    }

    public Builder setValidators(List<Validator<Table>> validators) {
      this.validators = validators;
      return this;
    }

  }

  public static final Direction DEFAULT_MENU_DIRECTION = Direction.BOTTOM;

  public static Builder builder() {
    return new Builder();
  }

  private final SheetConfig sheetConfig;
  private final Direction menuDirection;
  private final ImmutableCollection<MenuConfig> topMenuConfigs;
  private final ImmutableList<Validator<Table>> validators;
  private final TableDataConfig dataConfig;
  private final ParserConfig parserConfig;

  private final Traverser<MenuConfig> MENU_CONFIG_TRAVERSER =
      Traverser.forTree(new SuccessorsFunction<MenuConfig>() {
        @Override
        public Iterable<? extends MenuConfig> successors(MenuConfig node) {
          return node.getChildrens();
        }
      });

  private TableConfig(Builder builder) {
    super(builder);
    sheetConfig = builder.sheetConfig;
    menuDirection = Optional.ofNullable(builder.menuDirection).orElse(DEFAULT_MENU_DIRECTION);
    topMenuConfigs = builder.topMenuConfigBuilders.stream()
        .peek(topMenuConfigBuilder -> topMenuConfigBuilder.setTableConfig(this))
        .map(MenuConfig.Builder::build).collect(ImmutableSet.toImmutableSet());
    validators = builder.validators.stream().collect(ImmutableList.toImmutableList());
    dataConfig = builder.dataConfigBuilder.build();
    parserConfig = builder.parserConfig;
  }

  public Table.Builder parse(Sheet sheet) {
    Table.Builder builder = Table.builder().setConfig(this);

    // if (matcher.test(sheet)) {
    // builder.setTableBuilders(tableBuilders);
    // } else {
    // builder.setErrors(ImmutableList.of(new SheetError(sheet, matcher.getMessage(sheet))));
    // }
    return builder;
  }

  public ParserConfig getEffectiveParserConfig() {
    return Optional.ofNullable(parserConfig).orElse(sheetConfig.getEffectiveParserConfig());
  }

  public Stream<MenuConfig> getMenuConfigs() {
    return topMenuConfigs.stream().map(MENU_CONFIG_TRAVERSER::breadthFirst)
        .flatMap(Streams::stream);
  }

  public SheetConfig getSheetConfig() {
    return sheetConfig;
  }

  public ImmutableCollection<MenuConfig> getTopMenuConfigs() {
    return topMenuConfigs;
  }


  public Direction getMenuDirection() {
    return menuDirection;
  }

  public ParserConfig getParserConfig() {
    return parserConfig;
  }

  public TableDataConfig getDataConfig() {
    return dataConfig;
  }

  public ImmutableList<Validator<Table>> getValidators() {
    return validators;
  }

}
