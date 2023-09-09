import React, { ReactNode } from 'react';
import { createNativeStackNavigator } from '@react-navigation/native-stack';
import { NavigationContainer } from '@react-navigation/native';

import { NavProvider } from './screens/Nav/view';

import { RootViews } from './RootViews';
import Forecast from './screens/Forecast/view';
import Journals from './screens/Journals/view';

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
  );
};

export default MainLoop;