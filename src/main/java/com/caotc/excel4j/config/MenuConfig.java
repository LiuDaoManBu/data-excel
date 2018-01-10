package com.caotc.excel4j.config;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import org.apache.commons.collections4.CollectionUtils;
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.constant.MenuNecessity;
import com.caotc.excel4j.constant.MenuType;
import com.caotc.excel4j.matcher.usermodel.StandardCellMatcher;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;
import com.google.common.reflect.TypeToken;

public class MenuConfig<V> {
  public static class Builder<V> {
    private TableConfig tableConfig;
    private MenuConfig<?> parentMenuConfig;
    private DataConfig.Builder<V> dataConfigBuilder;
    private List<MenuConfig.Builder<?>> childrenMenuConfigBuilders;
    // 菜单匹配器
    private StandardCellMatcher.Builder matcherBuilder;
    // 第一个数据单元格相对于菜单单元格的单元格距离
    private Integer distance;
    private MenuNecessity menuNecessity;
    private Direction direction;
    // TODO need enum?
    private MenuType menuType;
    private ParserConfig parserConfig;

    public MenuConfig<V> build() {
      distance=Optional.ofNullable(distance).orElse(1);
      direction = Optional.ofNullable(direction).orElse(parentMenuConfig.direction);
      tableConfig = Optional.ofNullable(tableConfig).orElse(parentMenuConfig.tableConfig);
      parserConfig = Optional.ofNullable(parserConfig).orElse(ParserConfig.GLOBAL);
      // TODO 提示语
      Preconditions.checkState(Objects.nonNull(tableConfig));
      Preconditions.checkNotNull(matcherBuilder);
      Preconditions.checkNotNull(menuNecessity);
      Preconditions.checkState(Objects.nonNull(direction));
      Preconditions.checkNotNull(menuType);
      Preconditions.checkState(
          !(CollectionUtils.isNotEmpty(childrenMenuConfigBuilders) && Objects.nonNull(dataConfigBuilder)));
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

    public MenuConfig<?> getParentMenuConfig() {
      return parentMenuConfig;
    }

    public Builder<V> setParentMenuConfig(MenuConfig<?> parentMenuConfig) {
      this.parentMenuConfig = parentMenuConfig;
      return this;
    }

    public DataConfig.Builder<V> getDataConfigBuilder() {
      return dataConfigBuilder;
    }

    public Builder<V> setDataConfigBuilder(DataConfig.Builder<V> dataConfigBuilder) {
      this.dataConfigBuilder = dataConfigBuilder;
      return this;
    }

    public List<MenuConfig.Builder<?>> getChildrenMenuConfigBuilders() {
      return childrenMenuConfigBuilders;
    }

    public Builder<V> setChildrenMenuConfigBuilders(
        List<MenuConfig.Builder<?>> childrenMenuConfigBuilders) {
      this.childrenMenuConfigBuilders = childrenMenuConfigBuilders;
      return this;
    }

    public StandardCellMatcher.Builder getMatcherBuilder() {
      return matcherBuilder;
    }

    public Builder<V> setMatcherBuilder(StandardCellMatcher.Builder matcherBuilder) {
      this.matcherBuilder = matcherBuilder;
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
    return new Builder<V>().setDistance(DEFAULT_DISTANCE).setMenuNecessity(DEFAULT_MENU_NECESSITY);
  }

  private final TableConfig tableConfig;
  // 菜单匹配器
  private final StandardCellMatcher menuMatcher;
  // 第一个数据单元格相对于菜单单元格的单元格距离
  private final int distance;
  private final MenuNecessity menuNecessity;
  private final Direction direction;
  private final MenuType menuType;
  private final MenuConfig<?> parentMenuConfig;
  private final ImmutableCollection<MenuConfig<?>> childrenMenuConfigs;
  private final DataConfig<V> dataConfig;
  private final ParserConfig parserConfig;

  public MenuConfig(Builder<V> builder) {
    tableConfig = builder.tableConfig;
    menuMatcher = builder.matcherBuilder.build();
    distance = builder.distance;
    menuNecessity = builder.menuNecessity;
    direction = builder.direction;
    menuType = builder.menuType;
    parentMenuConfig = builder.parentMenuConfig;
    childrenMenuConfigs = builder.childrenMenuConfigBuilders.stream()
        .peek(childrenMenuConfigBuilder -> childrenMenuConfigBuilder.setParentMenuConfig(this))
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
    return parentMenuConfig == null;
  }

  public boolean isMustMenu() {
    return MenuNecessity.MUST.equals(menuNecessity);
  }

  public boolean isNotMustMenu() {
    return MenuNecessity.NOT_MUST.equals(menuNecessity);
  }

  public boolean isDataMenu() {
    return Iterables.isEmpty(childrenMenuConfigs);
  }

  public boolean isFixedDataMenu() {
    return isDataMenu() && LoadType.FIXED.equals(dataConfig.getLoadType());
  }

  public boolean isUnFixedDataMenu() {
    return isDataMenu() && LoadType.UNFIXED.equals(dataConfig.getLoadType());
  }

  public boolean isMixedDataMenu() {
    return isDataMenu() && LoadType.MIXED.equals(dataConfig.getLoadType());
  }

  // delegate methods start

  public boolean matches(StandardCell cell) {
    return menuMatcher.test(cell);
  }

  public boolean matches(Object value) {
    return dataConfig.matches(value);
  }

  public boolean support(Object value) {
    return dataConfig.support(value);
  }

  public V cast(Object value) {
    return dataConfig.cast(value);
  }

  public <T> boolean canCast(Class<T> clazz) {
    return dataConfig.canCast(clazz);
  }

  public <T> T cast(Object value, Class<T> clazz) {
    return dataConfig.cast(value, clazz);
  }

  public ImmutableCollection<TypeToken<?>> canCastTypes() {
    return dataConfig.canCastTypes();
  }

  public <T> boolean canCast(TypeToken<T> type) {
    return dataConfig.canCast(type);
  }

  public <T> T cast(Object value, TypeToken<T> type) {
    return dataConfig.cast(value, type);
  }

  // delegate methods end

  public MenuType getMenuType() {
    return menuType;
  }

  public StandardCellMatcher getMenuMatcher() {
    return menuMatcher;
  }

  public int getDistance() {
    return distance;
  }

  public MenuConfig<?> getParentMenuConfig() {
    return parentMenuConfig;
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

  public ImmutableCollection<MenuConfig<?>> getChildrenMenuConfigs() {
    return childrenMenuConfigs;
  }

  public TableConfig getTableConfig() {
    return tableConfig;
  }

  public ParserConfig getParserConfig() {
    return parserConfig;
  }

}
