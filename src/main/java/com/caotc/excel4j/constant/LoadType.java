package com.caotc.excel4j.constant;

import java.util.Collection;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;

import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;

public enum LoadType {
	DYNAMIC {
		@Override
		public void loadChildren(Menu menu) {
			
		}
	},FIXED {
		@Override
		public void loadChildren(Menu menu) {
			MenuConfig config=menu.getCheckMenuConfig();
			Collection<MenuConfig> childrenConfigs=config.getChildrenMenuConfigs();
			if(!CollectionUtils.isEmpty(childrenConfigs)){
				List<StandardCell> menuCells=config.getDirection().getCells(menu.getCell(), config.getFirstDistance());
				if(!CollectionUtils.isEmpty(menuCells)){
					childrenConfigs.forEach(childrenConfig->{
						menuCells.stream().filter(menuCell->childrenConfig.getMenuMatcher().matches(menuCell)).findFirst();
					});
				}
			}
		}
	};
	public abstract void loadChildren(Menu menu);
}
