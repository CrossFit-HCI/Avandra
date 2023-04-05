import React, { ReactNode } from 'react';
import {Button, Text, View} from 'react-native';
import { NativeStackScreenProps, createNativeStackNavigator } from '@react-navigation/native-stack';

import Nav from "./Nav";
import { NavigationContainer } from '@react-navigation/native';

type AVRootStackParams = {
  Forecast: undefined;
  JournalList: undefined;
}

type ForecastScreenProps = NativeStackScreenProps<AVRootStackParams, 'Forecast'>;

const Forecast = ({ route, navigation }:ForecastScreenProps) => {
  return (
      <View
        style={{
          flex: 1,
          flexDirection: 'column',
          justifyContent: 'center',
          alignItems: 'center',
          backgroundColor: 'purple',              
        }
      }> 
        <Text 
          style={{
            color: 'white',
            fontSize: 20,
          }}>Forecast</Text>

        <Nav>
          <Button
            onPress={() => navigation.navigate('JournalList')}
            title="Journals"
          />
        </Nav>                
      </View>
  )
}

type JournalListScreenProps = NativeStackScreenProps<AVRootStackParams, 'JournalList'>;

const JournalList = ({ route, navigation }:JournalListScreenProps) => {
  return (
    <View
      style={{
        flex: 1,
        justifyContent: 'center',
        alignItems: 'center',
    }}>
      <Text>Journal List</Text>

      <Nav>
          <Text>Journal List Nav</Text>

          <Button
            onPress={() => navigation.navigate('Forecast')}
            title="Forecast"
          />
      </Nav>
    </View>
  )
};

export interface MainProps {
  children: ReactNode;
}

const AVScreenStack = createNativeStackNavigator<AVRootStackParams>();

const MainLoop = () => {    
  return (
    <NavigationContainer>
      <AVScreenStack.Navigator screenOptions={{headerShown: false }}>
          <AVScreenStack.Screen name="Forecast" component={Forecast} />          
          <AVScreenStack.Screen name="JournalList" component={JournalList}/>
      </AVScreenStack.Navigator>
    </NavigationContainer>
  )
};

export default MainLoop;