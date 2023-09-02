import React, { ReactNode } from 'react';
import { Text, View} from 'react-native';
import { NativeStackScreenProps, createNativeStackNavigator } from '@react-navigation/native-stack';
import { NavigationContainer } from '@react-navigation/native';

import Nav, { NavProvider, NavScreen, NaviButton } from "./nav/view";
import { CloseNavModal, OpenNavModal } from './nav/model';

import { RootViews } from './RootViews';
import Forecast from "./forecast/view";
import Journals from "./journals/view"

export interface MainProps {
  children: ReactNode;
}

const AVScreenStack = createNativeStackNavigator<RootViews>();

const MainLoop = () => {    
  return (
    <NavProvider>
      <NavigationContainer>
        <AVScreenStack.Navigator screenOptions={{headerShown: false }}>
            <AVScreenStack.Screen name="Forecast" component={Forecast} />          
            <AVScreenStack.Screen name="Journals" component={Journals}/>
        </AVScreenStack.Navigator>
      </NavigationContainer>
    </NavProvider>
  )
};

export default MainLoop;