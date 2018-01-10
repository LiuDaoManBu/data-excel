package com.caotc.excel4j.config;

import java.util.List;
import java.util.Optional;
import com.caotc.excel4j.constant.Direction;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Streams;
import com.google.common.graph.SuccessorsFunction;
import com.google.common.graph.Traverser;

public class TableConfig {
  public static class Builder {
    private List<MenuConfig.Builder<?>> topMenuConfigBuilders;
    private SheetConfig sheetConfig;
    // TODO Table没有Matcher?
    private Direction fixedMenuDirection;
    private Direction unFixedMenuDirection;
    private ParserConfig parserConfig;

    public TableConfig builder() {
      parserConfig = Optional.ofNullable(parserConfig).orElse(ParserConfig.GLOBAL);
      return new TableConfig(this);
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

    public List<MenuConfig.Builder<?>> getTopMenuConfigBuilders() {
      return topMenuConfigBuilders;
    }

    public Builder setTopMenuConfigBuilders(List<MenuConfig.Builder<?>> topMenuConfigBuilders) {
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

  }

  private static final Traverser<MenuConfig<?>> MENU_CONFIG_TRAVERSER =
      Traverser.forTree(new SuccessorsFunction<MenuConfig<?>>() {
        @Override
        public Iterable<? extends MenuConfig<?>> successors(MenuConfig<?> node) {
          return node.getChildrenMenuConfigs();
        }
      });
  private final SheetConfig sheetConfig;
  private final Direction fixedMenuDirection;
  private final Direction unFixedMenuDirection;
  private final ImmutableCollection<MenuConfig<?>> topMenuConfigs;
  private final ParserConfig parserConfig;

  private final ImmutableCollection<MenuConfig<?>> menuConfigs;

  public TableConfig(Builder builder) {
    sheetConfig = builder.sheetConfig;
    fixedMenuDirection = builder.fixedMenuDirection;
    unFixedMenuDirection = builder.unFixedMenuDirection;


    // topMenuConfigs = Collections2.filter(menuConfigs, MenuConfig::isTopMenu);
    topMenuConfigs = builder.topMenuConfigBuilders.stream()
        .peek(topMenuConfigBuilder -> topMenuConfigBuilder.setTableConfig(this))
        .map(MenuConfig.Builder::build).collect(ImmutableSet.toImmutableSet());
    parserConfig = builder.parserConfig;

    // menuConfigs =
    // builder.menuConfigBuilders.stream().map(MenuConfig.Builder::build).collect(ImmutableSet.toImmutableSet());
    menuConfigs = topMenuConfigs.stream().map(MENU_CONFIG_TRAVERSER::breadthFirst)
        .flatMap(Streams::stream).collect(ImmutableSet.toImmutableSet());
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

  public ImmutableCollection<MenuConfig<?>> getMenuConfigs() {
    return menuConfigs;
  }

  public ParserConfig getParserConfig() {
    return parserConfig;
  }

}
