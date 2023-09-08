import { TextStyle, ViewStyle } from 'react-native';
import { borderTheme, IconTheme, iconTheme, ButtonTheme, cardHeaderButtonTheme } from './theme';

interface IconStyle extends IconTheme {
    iconSize: number
}

interface HeaderTheme extends ViewStyle {
    headerTextForecastContainer?: ViewStyle,
    headerTextContainer?: ViewStyle,
    headerText: TextStyle
    completedContainer: ViewStyle,
    buttonTheme: ButtonTheme,
    iconTheme: IconStyle
}

interface CardTheme extends ViewStyle {    
    header: HeaderTheme,
};

export const cardTheme: CardTheme = {
    flex: 1,
    width: '90%',
    maxHeight: '82%',
    marginTop: 15,
    borderColor: borderTheme.card.borderColor,
    borderWidth: borderTheme.card.borderWidth,
    header: {
        flex: 1,
        flexDirection: 'row',
        justifyContent: 'center',
        alignItems: 'center',        
        maxHeight: 40,        
        backgroundColor: 'rgba(196,196,196,1)',
        borderColor: borderTheme.card.borderColor,
        borderWidth: borderTheme.card.borderWidth,
        headerTextForecastContainer: {
            flex: 1,
            flexDirection: 'row',
            justifyContent: 'center'
        },
        headerTextContainer: {
            flex: 1,
            padding: 5,
            flexDirection: 'row',
            justifyContent: 'flex-start'            
        },
        headerText: {
            fontSize: 22,
            fontWeight: 'bold'
        },
        completedContainer: {
            flex: 1,
            maxWidth: 50,
            height: '100%',
            alignItems: 'center',
            justifyContent: 'center'
        },
        buttonTheme: cardHeaderButtonTheme,
        iconTheme: {
            iconSize: 30,
            completed: iconTheme.completed,
            uncompleted: iconTheme.uncompleted
        }
    }
};
