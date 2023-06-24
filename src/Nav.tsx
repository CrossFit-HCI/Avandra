import React, { ReactElement, ReactNode, createElement, useContext, useEffect } from 'react';
import { Button, View } from 'react-native';
import NavProvider, { NavContext, NavScreenProps, getNavScreen, createNavContext, NaviButton, useAppDispatch, openNav, toggleNav, useAppSelector, isNavOpenedSelector, currentScreenSelector, linkStackScreens, linkModalScreens  } from "./NavViewModel";
import { navContainerBarViewStyle, navContainerViewStyle } from './NavStyles';

const NavView = () => {  
  const context = useContext(NavContext);
  
  const isNavOpened = useAppSelector((state) => isNavOpenedSelector(state));
  const currentScreen = useAppSelector((state) => getNavScreen(context, state));    

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

export const NavScreen = ({label, screen}:NavScreenProps) => { 
  return createElement('NavScreen', {label: label, screen: screen}, <View/>) 
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
  const context: NavContext = createNavContext(main, children);

  useEffect(() => {   
    // 2. Link the context with the state:
    context.stacks.map((stack, index) => {
      dispatch(linkStackScreens({key: stack.label, value: index}));                    
    });
    context.modals.map((modal, index) => {
      dispatch(linkModalScreens({key: modal.label, value: index}));                    
    })
  });  

  // 3. Render the main screen:
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
