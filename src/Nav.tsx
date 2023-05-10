import React, { ReactNode, createElement, useContext } from 'react';
import { Button, View } from 'react-native';
import NavProvider, { NavContext, NavScreenProps, getNavScreen, createNavContext, NaviButton, useAppDispatch, openNav, toggleNav, useAppSelector, isNavOpenedSelector, groupTypeSelector  } from "./NavViewModel";
import { navContainerBarViewStyle, navContainerViewStyle } from './NavStyles';

const NavView = () => {  
  const context = useContext(NavContext);
  const state = useAppSelector((state) => state);
  
  const isNavOpened = isNavOpenedSelector(state);
  const currentScreen = getNavScreen(context, state);

  // Used to hide and show part of the Nav; this mimics the Nav being open or
  // closed.
  let navHeight = '50%';
  let navBottom = isNavOpened ? '0%' : '-42%';  

  const dispatch = useAppDispatch();
  const togNav = () => dispatch(toggleNav());  

  return (    
      <View style={navContainerViewStyle(navHeight, navBottom).container}>          
        <View
          style={navContainerBarViewStyle.container}>                    
          <Button 
            title="Toggle Nav"  onPress={() => {togNav()}}/>
        </View>        
        {currentScreen}
      </View>      
  )
}

export const NavScreen = ({id, screen}:NavScreenProps) => { 
  return createElement('NavScreen', {id: id, screen: screen}, <View/>) 
}

export interface NavScreensProps {
  children: ReactNode;
}

export const Nav = ({children}:NavScreensProps) => {  
  // 1. Extract the screen data from each of the children.  
  const context: NavContext = createNavContext(children);

  // 3. Make sure the Nav is open:
  const dispatch = useAppDispatch();
  const navOpen = () => dispatch(openNav());

  navOpen();

  // 4. Render the main screen:
  return (
    <NavContext.Provider value={context}>
      <NavView />
    </NavContext.Provider>            
  );
}

/* We don't want library users to have access to the view model. So we export 
 * the provider here. */
export { NavProvider, NaviButton };
export default Nav;
