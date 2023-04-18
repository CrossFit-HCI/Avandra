import React, { ReactNode, Children, useContext } from 'react';
import {Button, View} from 'react-native';
import { toggleNav, useAppDispatch, useAppSelector, NavScreenState, injectScreens, setMainView, NavContext, openNav, NavGroupType, NavGroupState, initialNavContext } from "./NavState";
import { navContainerBarViewStyle, navContainerViewStyle } from './NavStyles';
import navStore, { RootState } from './NavStore';
import { Provider } from 'react-redux';

const NavView = () => {
  // Setup the Nav's toggle button's callback:
  const navStatus = useAppSelector((state:RootState) => state.nav.navStatus);  
  const dispatch = useAppDispatch();    
  const toggleNavStatus = () => dispatch(toggleNav())

  // Setup the current screen of the Nav:
  const screens = useContext(NavContext);
  const screenPtr: number = useAppSelector((state:RootState) => {
    return state.nav.screensPtr;
  });
  // XXX: Need to update this:
  const currentScreen = screens.mainScreen.view;

  // Used to hide and show part of the Nav; this mimics the Nav being open or
  // closed.
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

const extractScreens = (acc:NavContext, child: ReactNode) => {    
  if (React.isValidElement(child)) {
    // 1. Make sure that we are in a NavScreen component:
    if (child.type == 'NavScreen') {        
      // 2: child should have it's own children:      
      const screens = Children.toArray(child.props.children).reduce<NavContext>((acc, childScreen) => {
        // 3. Pattern match over the possible screen groups:
        switch (child.type) {
          case NavGroupType.MainScreen:
            // 3.a. Extract the main screen:
            acc.mainScreen = child.props.screen;

          case NavGroupType.NavStack:
            // 3.b: Extract the stack screens:
            

          case NavGroupType.NavModals:
            // 3.c: Extract the modal screens:
        }
        return acc;
      }, initialNavContext)
      

      return screens;
    } else {
      throw new Error (`A component in the Nav has the wrong type: ${child.type}.\n
         Only NavScreen can be used in the Nav.`)
    }      
  } else {
    throw new Error ('A NavScreen is the only component allowable in the Nav.')
  }    
}

export const NavScreens = ({children}:NavProps) => {
  const dispatch = useAppDispatch();    
  const openTheNav = () => dispatch(openNav());
  const injectTheScreens = (screensLength: number) => dispatch(injectScreens(screensLength));

  // 1. Extract the screen data from each of the children.  
  const screens: NavContext = Children.toArray(children).reduce<NavContext>(extractScreens, initialNavContext);  

  // 2. Inject the screens into the Nav:
  //injectTheScreens(screens.length);
  // 3. Make sure the Nav is open:
  openTheNav();

  // 4. Render the top-most view in screens:
  return (
    <NavContext.Provider value={screens}>
      <NavView />
    </NavContext.Provider>          
  );
}

export default NavScreens;
