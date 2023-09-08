import React from 'react';
import { Pressable, Text, View} from 'react-native';
import { NativeStackScreenProps } from '@react-navigation/native-stack';
import Icon from 'react-native-vector-icons/MaterialCommunityIcons';

import Nav, { NavScreen, NaviButton } from "../nav/view";
import { CloseNavModal } from '../nav/model';
import { navCustomViewStyleSheet } from '../nav/styles';

import { RootViews } from '../RootViews';
import { forecastStyle } from './styles';
import { Card } from '../components/Card/card';

import { Journal, TrainingBlock, getJournal } from '../model';
import { Hrule } from '../components/hrule';

type ForecastScreenProps = NativeStackScreenProps<RootViews, 'Forecast'>;

const ModalScreen = () => {
  return (
    <View>
      <Text style={ forecastStyle.heading1 }>I'm a modal.</Text>

      <CloseNavModal title='Close Modal1'/>
    </View>
  )
}

const CardForecastHeader = (journalTitle: string, markCompleted: boolean) => { 
  let headerTheme = forecastStyle.component.card.header;
  let iconCompleted = headerTheme.iconTheme.completed;
  let iconUncompleted = headerTheme.iconTheme.uncompleted;
  let icon = markCompleted ? iconCompleted : iconUncompleted;

  return (
      <>
          <View style={ headerTheme.completedContainer }>
              <Icon name={ icon } size={headerTheme.iconTheme.iconSize} />
          </View>
          <View style={ headerTheme.headerTextForecastContainer }>
              <Text style={ headerTheme.headerText }>{journalTitle}</Text>
          </View>
          <Pressable style={ headerTheme.buttonTheme.container }>
              <Text style={ headerTheme.buttonTheme.text }>Go!</Text>
          </Pressable>
      </>
  );
};

interface CardForecastProps {
  journal: Journal
};

interface TrainingBlockListViewProps {
  trainingBlocks: TrainingBlock[]
};

const TrainingBlockListView = ({ trainingBlocks }: TrainingBlockListViewProps) => {
  let totalTrainingBlocks: number = trainingBlocks.length;
  /* We can only fit 12 training block entires with the current dimensions. So
   * we end up with 11 horizontal rules total. */
  let totalEntries: number = totalTrainingBlocks <= 12 ? totalTrainingBlocks : 12;

  return (
    Array.from({length: totalEntries}, (_, i) => i).map((i) => {
      let index: string = i+1 < 10 ? ` ${i+1}` : `${i+1}`;      
      let hrule: JSX.Element = i < totalEntries - 1 ? <Hrule /> : <></>;
      
      return (
        <>
        <Text style={{fontSize: 16}}>{index}. {trainingBlocks[i].description}</Text>
        {hrule}
        </>
      );
    }));
};

const CardForecast = ({ journal }: CardForecastProps) => {  
  return (
    <Card header={CardForecastHeader(journal.title, false)}>
      <View style={{flex: 1, gap: 10, padding: 10}}>
        <TrainingBlockListView trainingBlocks={journal.trainingBlocks} />
      </View>
    </Card>
  );
};

const Forecast = ({ navigation }: ForecastScreenProps) => {
  let navMainScreen = () => {
    return (
      <View style={navCustomViewStyleSheet.container}>
        <NaviButton
          onPress={() => navigation.navigate('Journals')}
          title="Journals" />         
      </View>
    );
  };

  let journal: Journal = getJournal();

  /* How to open a modal: 
   * <OpenNavModal label='Modal1' title="Open Modal1" />  */
  return (
      <View style={ forecastStyle.component }> 
        <Text style={{fontSize: 35}}>Today (09/06)</Text>
        
        <CardForecast journal={journal} />

        <Nav main={navMainScreen()}>
            <NavScreen label='Modal1'  screen={ModalScreen()} />
        </Nav>                
      </View>
  );
};

export default Forecast;