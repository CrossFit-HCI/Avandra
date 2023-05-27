import React, { ReactElement, ReactNode, createElement, useContext } from 'react';
import { Button, View } from 'react-native';
import NavProvider, { NavContext, NavScreenProps, getNavScreen, createNavContext, NaviButton, useAppDispatch, openNav, toggleNav, useAppSelector, isNavOpenedSelector, groupTypeSelector, linkStackScreens, linkModalScreens  } from "./NavViewModel";
import { navContainerBarViewStyle, navContainerViewStyle } from './NavStyles';

const NavView = () => {  
  const context = useContext(NavContext);
  const state = useAppSelector((state) => state);
  
  const isNavOpened = isNavOpenedSelector(state);
  const currentScreen = getNavScreen(context, state);    

  const dispatch = useAppDispatch();
  const togNav = () => dispatch(toggleNav());  

  return (    
      <View style={navContainerViewStyle(isNavOpened).container}>          
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

interface NavModalsProps {
  children: ReactNode;
}

export const NavModals = ({children}:NavModalsProps): JSX.Element => {
  return createElement('NavModals', {children: children}, <View/>) 
}

export interface NavScreensProps {
  children: ReactNode;
  main: ReactElement;
}

export const Nav = ({children, main}:NavScreensProps) => {  
  const dispatch = useAppDispatch();

  // 1. Extract the screen data from each of the children.  
  // TODO: Add main to the parameters for createNavContext:
  const context: NavContext = createNavContext(main, children);
  // 2. Link the context with the state:
  dispatch(linkStackScreens({payload: context.stacks}));
  dispatch(linkModalScreens({payload: context.modals}));

  // 3. Make sure the Nav is open:  
  dispatch(openNav());

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
