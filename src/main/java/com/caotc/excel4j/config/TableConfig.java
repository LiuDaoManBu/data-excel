package com.caotc.excel4j.config;

import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import org.apache.poi.ss.usermodel.Sheet;
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.matcher.Matcher;
import com.caotc.excel4j.parse.result.Table;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Streams;
import com.google.common.graph.SuccessorsFunction;
import com.google.common.graph.Traverser;

// TODO classType?
public class TableConfig<V> {
  public static class Builder<V> {
    private List<MenuConfig.Builder<?>> topMenuConfigBuilders;
    private SheetConfig sheetConfig;
    private Matcher.Builder<Table<V>> matcherBuilder;
    private Direction fixedMenuDirection;
    private Direction unFixedMenuDirection;
    private TableDataConfig<V> dataConfig;
    private ParserConfig parserConfig;

    public TableConfig<V> build() {
      return new TableConfig<>(this);
    }

    public SheetConfig getSheetConfig() {
      return sheetConfig;
    }

    public Builder<V> setSheetConfig(SheetConfig sheetConfig) {
      this.sheetConfig = sheetConfig;
      return this;
    }

    public Direction getFixedMenuDirection() {
      return fixedMenuDirection;
    }

    public Builder<V> setFixedMenuDirection(Direction fixedMenuDirection) {
      this.fixedMenuDirection = fixedMenuDirection;
      return this;
    }

    public Direction getUnFixedMenuDirection() {
      return unFixedMenuDirection;
    }

    public Builder<V> setUnFixedMenuDirection(Direction unFixedMenuDirection) {
      this.unFixedMenuDirection = unFixedMenuDirection;
      return this;
    }

    public List<MenuConfig.Builder<?>> getTopMenuConfigBuilders() {
      return topMenuConfigBuilders;
    }

    public Builder<V> setTopMenuConfigBuilders(
        List<MenuConfig.Builder<?>> topMenuConfigBuilders) {
      this.topMenuConfigBuilders = topMenuConfigBuilders;
      return this;
    }

    public ParserConfig getParserConfig() {
      return parserConfig;
    }

    public Builder<V> setParserConfig(ParserConfig parserConfig) {
      this.parserConfig = parserConfig;
      return this;
    }

    public Matcher.Builder<Table<V>> getMatcherBuilder() {
      return matcherBuilder;
    }

    public Builder<V> setMatcherBuilder(Matcher.Builder<Table<V>> matcherBuilder) {
      this.matcherBuilder = matcherBuilder;
      return this;
    }

    public TableDataConfig<V> getDataConfig() {
      return dataConfig;
    }

    public Builder<V> setDataConfig(TableDataConfig<V> dataConfig) {
      this.dataConfig = dataConfig;
      return this;
    }

  }

  public static <V> Builder<V> builder() {
    return new Builder<>();
  }

  private final SheetConfig sheetConfig;
  private final Direction fixedMenuDirection;
  private final Direction unFixedMenuDirection;
  private final ImmutableCollection<MenuConfig<?>> topMenuConfigs;
  private final Matcher<Table<V>> matcher;
  private final TableDataConfig<V> dataConfig;
  private final ParserConfig parserConfig;

  private final Traverser<MenuConfig<?>> MENU_CONFIG_TRAVERSER =
      Traverser.forTree(new SuccessorsFunction<MenuConfig<?>>() {
        @Override
        public Iterable<? extends MenuConfig<?>> successors(MenuConfig<?> node) {
          return node.getChildrens();
        }
      });

  private TableConfig(Builder<V> builder) {
    sheetConfig = builder.sheetConfig;
    fixedMenuDirection = builder.fixedMenuDirection;
    unFixedMenuDirection = builder.unFixedMenuDirection;
    // TODO
    matcher = Optional.ofNullable(builder.matcherBuilder).map(Matcher.Builder::build).orElse(null);

    topMenuConfigs = builder.topMenuConfigBuilders.stream()
        .peek(topMenuConfigBuilder -> topMenuConfigBuilder.setTableConfig(this))
        .map(MenuConfig.Builder::build).collect(ImmutableSet.toImmutableSet());
    dataConfig = builder.dataConfig;
    parserConfig = builder.parserConfig;

    // menuConfigs =
    // builder.menuConfigBuilders.stream().map(MenuConfig.Builder::build).collect(ImmutableSet.toImmutableSet());
  }

  public Table.Builder<V> parse(Sheet sheet) {
    Table.Builder<V> builder = Table.<V>builder().setTableConfig(this);

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

  public Stream<MenuConfig<?>> getMenuConfigs() {
    return topMenuConfigs.stream().map(MENU_CONFIG_TRAVERSER::breadthFirst)
        .flatMap(Streams::stream);
  }

  public SheetConfig getSheetConfig() {
    return sheetConfig;
  }

  public ImmutableCollection<MenuConfig<?>> getTopMenuConfigs() {
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

  public TableDataConfig<V> getDataConfig() {
    return dataConfig;
  }

  public Matcher<Table<V>> getMatcher() {
    return matcher;
  }

}
