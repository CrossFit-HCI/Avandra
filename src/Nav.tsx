import React, { ReactNode, Children, createContext, useContext } from 'react';
import {Button, View} from 'react-native';
import { toggleNav, useAppDispatch, useAppSelector, NavScreenState, injectScreens, setMainView, initialNavContext } from "./NavState";
import { navContainerBarViewStyle, navContainerViewStyle } from './NavStyles';
import navStore, { RootState } from './NavStore';
import { Provider } from 'react-redux';

interface NavViewProps {
  children: ReactNode
}

const NavView = () => {
  const navStatus = useAppSelector((state:RootState) => state.nav.navStatus);  
  const currentScreen: React.ReactNode = useAppSelector((state:RootState) => {
    let currentScreen = state.nav.currentScreen;
    if (currentScreen) {
      return currentScreen.view;
    } else {
      throw new Error ('NavView/currentScreen: currentScreen is undefined.');
    }
  });
  const dispatch = useAppDispatch();    
  const toggleNavStatus = () => dispatch(toggleNav())

  let navHeight = '50%';
  let navBottom = navStatus.status == 'opened' ? '0%' : '-42%';  

  return (    
      <View style={navContainerViewStyle(navHeight, navBottom).container}>
          
        <View
          style={navContainerBarViewStyle.container}>                    
          <Button 
            title="Toggle Nav"  onPress={toggleNavStatus}/>
        </View>        
        {currentScreen}
      </View>      
  )
}

export interface NavCommContainerProps {
  children: ReactNode;
}

export const NavCommContainer = ({ children } : NavCommContainerProps) => {
  return (
    <Provider store={navStore}>      
      {children}
    </Provider>
  )
}

export interface NavScreenProps {
  id: string;
  screen: ReactNode;
}

export const NavScreen = ({id, screen}:NavScreenProps) => null;

export interface NavProps {
  children: ReactNode;
}

export const NavStackComp = ({children}:NavProps) => {
  /*
   * 1. Extract the screen data from each of the children.
   * 2. Push the screens into the store.
   * 3. Open the top-most screen in the Nav.
   */  
  const screens = Children.toArray(children).reduce<NavScreenState[]>((acc, child) => {
    if (React.isValidElement(child)) {
      if (child.type == 'NavScreen') {        
        // At this point we know child.props is a NavScreenState.
        // Push it onto the stack.
        acc.push(child.props);
        return acc;
      } else {
        throw new Error (`A component in the Nav has the wrong type: ${child.type}.\n
           Only NavScreen can be used in the Nav.`)
      }      
    } else {
      throw new Error ('A NavScreen is the only component allowable in the Nav.')
    }    
  }, [])
  const dispatch = useAppDispatch();  

  dispatch(injectScreens(screens));

  return (
      <NavView />          
  )
}

export default ({children}:NavProps) => {
  const dispatch = useAppDispatch();  

  // Setup the screens context with just `children` in it.
  const NavContext = createContext(initialNavContext);
  let mainScreen = {screens: [{id: 'main', view: children}]};

  // Setup the state.
  dispatch(setMainView());

  return (
    <NavContext.Provider value={mainScreen}>
      <NavView />
    </NavContext.Provider>
  )
}
