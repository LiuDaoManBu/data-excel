package com.caotc.excel4j.config;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;
import org.apache.poi.ss.usermodel.Sheet;
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.matcher.BaseValidator;
import com.caotc.excel4j.matcher.Validator;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.Table;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Streams;
import com.google.common.graph.SuccessorsFunction;
import com.google.common.graph.Traverser;

// TODO classType?
public class TableConfig {
  public static class Builder {
    private Object id;
    private List<MenuConfig.Builder> topMenuConfigBuilders;
    private SheetConfig sheetConfig;
    private Direction fixedMenuDirection;
    private Direction unFixedMenuDirection;
    private TableDataConfig dataConfig;
    private List<Validator<Table>> validators;
    private ParserConfig parserConfig;

    public TableConfig build() {
      return new TableConfig(this);
    }

    public Object getId() {
      return id;
    }

    public Builder setId(Object id) {
      this.id = id;
      return this;
    }

    public SheetConfig getSheetConfig() {
      return sheetConfig;
    }

    public Builder setSheetConfig(SheetConfig sheetConfig) {
      this.sheetConfig = sheetConfig;
      return this;
    }

    public Direction getFixedMenuDirection() {
      return fixedMenuDirection;
    }

    public Builder setFixedMenuDirection(Direction fixedMenuDirection) {
      this.fixedMenuDirection = fixedMenuDirection;
      return this;
    }

    public Direction getUnFixedMenuDirection() {
      return unFixedMenuDirection;
    }

    public Builder setUnFixedMenuDirection(Direction unFixedMenuDirection) {
      this.unFixedMenuDirection = unFixedMenuDirection;
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

    public TableDataConfig getDataConfig() {
      return dataConfig;
    }

    public Builder setDataConfig(TableDataConfig dataConfig) {
      this.dataConfig = dataConfig;
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

  public static Builder builder() {
    return new Builder();
  }

  private final Object id;
  private final SheetConfig sheetConfig;
  private final Direction fixedMenuDirection;
  private final Direction unFixedMenuDirection;
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
    id = builder.id;
    sheetConfig = builder.sheetConfig;
    fixedMenuDirection = builder.fixedMenuDirection;
    unFixedMenuDirection = builder.unFixedMenuDirection;
    topMenuConfigs = builder.topMenuConfigBuilders.stream()
        .peek(topMenuConfigBuilder -> topMenuConfigBuilder.setTableConfig(this))
        .map(MenuConfig.Builder::build).collect(ImmutableSet.toImmutableSet());
    // TODO 注释,重复匹配?tip,sheet可选是否必须匹配
    Validator<Table> validator = new BaseValidator<>(
        topMenuConfigs.stream().collect(ImmutableMap.toImmutableMap(topMenuConfig -> {
          Predicate<Table> predicate = table -> table.getTopMenus().stream().map(Menu::getConfig)
              .filter(topMenuConfig::equals).findAny().isPresent();
          return predicate;
        }, topMenuConfig -> {
          Function<Table, String> function = table -> topMenuConfig + "没有匹配到任何结果";
          return function;
        })));

    validators = Stream.concat(builder.validators.stream(), Stream.of(validator))
        .collect(ImmutableList.toImmutableList());
    dataConfig = builder.dataConfig;
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

  public Direction getFixedMenuDirection() {
    return fixedMenuDirection;
  }

  public Direction getUnFixedMenuDirection() {
    return unFixedMenuDirection;
  }

  public ParserConfig getParserConfig() {
    return parserConfig;
  }

  public TableDataConfig getDataConfig() {
    return dataConfig;
  }

  public Object getId() {
    return id;
  }

  public ImmutableList<Validator<Table>> getValidators() {
    return validators;
  }

}
