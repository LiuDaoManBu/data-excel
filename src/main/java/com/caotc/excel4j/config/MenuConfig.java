package com.caotc.excel4j.config;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.constant.MenuNecessity;
import com.caotc.excel4j.constant.MenuType;
import com.caotc.excel4j.matcher.Matcher;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;

public class MenuConfig<V> {
  public static class Builder<V> {
    private TableConfig tableConfig;
    private MenuConfig<?> parent;
    private DataConfig.Builder<V> dataConfigBuilder;
    private List<MenuConfig.Builder<?>> childrenBuilders;
    // 菜单匹配器
    private Matcher.Builder<StandardCell> matcherBuilder;
    // 第一个数据单元格相对于菜单单元格的单元格距离
    private Integer distance;
    private MenuNecessity menuNecessity;
    private Direction direction;
    // TODO need enum?
    private MenuType menuType;
    private ParserConfig parserConfig;

    public MenuConfig<V> build() {
      distance = Optional.ofNullable(distance).orElse(DEFAULT_DISTANCE);
      menuNecessity = Optional.ofNullable(menuNecessity).orElse(DEFAULT_MENU_NECESSITY);

      // tableConfig = Optional.ofNullable(tableConfig).orElse(parentMenuConfig.tableConfig);
      tableConfig = Optional.ofNullable(tableConfig)
          .orElse(Optional.ofNullable(parent).map(MenuConfig::getTableConfig).orElse(null));
      direction = Optional.ofNullable(direction)
          .orElse(Optional.ofNullable(parent).map(MenuConfig::getDirection).orElse(null));
      childrenBuilders = Optional.ofNullable(childrenBuilders).orElse(ImmutableList.of());
      // TODO tip
      Preconditions.checkState(Objects.nonNull(tableConfig));
      Preconditions.checkNotNull(matcherBuilder);
      Preconditions.checkNotNull(menuNecessity);
      // Preconditions.checkState(Objects.nonNull(direction));
      Preconditions.checkNotNull(menuType);
      Preconditions.checkState(
          !(!Iterables.isEmpty(childrenBuilders) && Objects.nonNull(dataConfigBuilder)));
      return new MenuConfig<V>(this);
    }

    public TableConfig getTableConfig() {
      return tableConfig;
    }

    public Builder<V> setTableConfig(TableConfig tableConfig) {
      this.tableConfig = tableConfig;
      return this;
    }

    public int getDistance() {
      return distance;
    }

    public Builder<V> setDistance(int distance) {
      this.distance = distance;
      return this;
    }

    public MenuNecessity getMenuNecessity() {
      return menuNecessity;
    }

    public Builder<V> setMenuNecessity(MenuNecessity menuNecessity) {
      this.menuNecessity = menuNecessity;
      return this;
    }

    public Direction getDirection() {
      return direction;
    }

    public Builder<V> setDirection(Direction direction) {
      this.direction = direction;
      return this;
    }

    public MenuType getMenuType() {
      return menuType;
    }

    public Builder<V> setMenuType(MenuType menuType) {
      this.menuType = menuType;
      return this;
    }

    public MenuConfig<?> getParent() {
      return parent;
    }

    public Builder<V> setParent(MenuConfig<?> parent) {
      this.parent = parent;
      return this;
    }

    public DataConfig.Builder<V> getDataConfigBuilder() {
      return dataConfigBuilder;
    }

    public Builder<V> setDataConfigBuilder(DataConfig.Builder<V> dataConfigBuilder) {
      this.dataConfigBuilder = dataConfigBuilder;
      return this;
    }

    public List<MenuConfig.Builder<?>> getChildrenBuilders() {
      return childrenBuilders;
    }

    public Builder<V> setChildrenBuilders(List<MenuConfig.Builder<?>> childrenBuilders) {
      this.childrenBuilders = childrenBuilders;
      return this;
    }


    public Matcher.Builder<StandardCell> getMatcherBuilder() {
      return matcherBuilder;
    }

    public Builder<V> setMatcherBuilder(Matcher.Builder<StandardCell> matcherBuilder) {
      this.matcherBuilder = matcherBuilder;
      return this;
    }

    public Builder<V> setDistance(Integer distance) {
      this.distance = distance;
      return this;
    }

    public ParserConfig getParserConfig() {
      return parserConfig;
    }

    public Builder<V> setParserConfig(ParserConfig parserConfig) {
      this.parserConfig = parserConfig;
      return this;
    }

  }

  private static final int DEFAULT_DISTANCE = 1;
  private static final MenuNecessity DEFAULT_MENU_NECESSITY = MenuNecessity.MUST;

  public static <V> Builder<V> builder() {
    return new Builder<>();
  }

  private final TableConfig tableConfig;
  // 菜单匹配器
  private final Matcher<StandardCell> menuMatcher;
  // 第一个数据单元格相对于菜单单元格的单元格距离
  private final int distance;
  private final MenuNecessity menuNecessity;
  private final Direction direction;
  private final MenuConfig<?> parent;
  private final ImmutableCollection<MenuConfig<?>> childrens;
  private final DataConfig<V> dataConfig;
  private final ParserConfig parserConfig;

  private MenuConfig(Builder<V> builder) {
    tableConfig = builder.tableConfig;
    menuMatcher = builder.matcherBuilder.build();
    distance = builder.distance;
    menuNecessity = builder.menuNecessity;
    direction = builder.direction;
    parent = builder.parent;
    childrens = builder.childrenBuilders.stream()
        .peek(childrenMenuConfigBuilder -> childrenMenuConfigBuilder.setParent(this))
        .map(Builder::build).collect(ImmutableSet.toImmutableSet());
    dataConfig = builder.dataConfigBuilder.setMenuConfig(this).build();
    parserConfig = builder.parserConfig;
  }

  public Optional<Field> getField() {
    return Optional.ofNullable(dataConfig).map(DataConfig::getField);
  }

  public Optional<String> getFieldName() {
    return Optional.ofNullable(dataConfig).map(DataConfig::getFieldName);
  }

  public boolean isTopMenu() {
    return Objects.isNull(parent);
  }

  public boolean isMustMenu() {
    return MenuNecessity.MUST.equals(menuNecessity);
  }

  public boolean isNotMustMenu() {
    return MenuNecessity.NOT_MUST.equals(menuNecessity);
  }

  public boolean isDataMenu() {
    return childrens.isEmpty();
  }

  public boolean isFixedDataMenu() {
    return isDataMenu() && LoadType.FIXED.equals(dataConfig.getLoadType());
  }

  public boolean isUnFixedDataMenu() {
    return isDataMenu() && LoadType.UNFIXED.equals(dataConfig.getLoadType());
  }

  public ParserConfig getEffectiveParserConfig() {
    return Optional.ofNullable(parserConfig).orElse(Optional.ofNullable(parent)
        .map(MenuConfig::getParserConfig).orElse(tableConfig.getEffectiveParserConfig()));
  }

  // delegate methods start

  public boolean matches(StandardCell cell) {
    return menuMatcher.test(cell);
  }

  // delegate methods end

  public Matcher<StandardCell> getMenuMatcher() {
    return menuMatcher;
  }

  public int getDistance() {
    return distance;
  }

  public MenuConfig<?> getParent() {
    return parent;
  }

  public Direction getDirection() {
    return direction;
  }

  public MenuNecessity getMenuNecessity() {
    return menuNecessity;
  }

  public DataConfig<V> getDataConfig() {
    return dataConfig;
  }

  public ImmutableCollection<MenuConfig<?>> getChildrens() {
    return childrens;
  }

  public TableConfig getTableConfig() {
    return tableConfig;
  }

  public ParserConfig getParserConfig() {
    return parserConfig;
  }

}
