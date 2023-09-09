import { Text, View } from 'react-native';
import { ReactNode } from 'react';

import { cardStyle } from './styles';

const CardBasicHeader = (description: string) => {    
    return (  
        <View style={ cardStyle.headerTextContainer }>
            <Text style={ cardStyle.headerText }>{description}</Text>
        </View>
    );
};

interface CardProps {
    header: ReactNode,
    children: ReactNode
}

export const Card = ({header, children}: CardProps) => {
    return (
        <View style={cardStyle.component}>
            <View style={cardStyle.headerComponent}>
                {header}
            </View>
            {children}
        </View>
    );
};

interface CardBasicProps {
    description: string,
    children: ReactNode
}

export const CardBasic = ({description, children}: CardBasicProps) => {
    return (
        <Card header={CardBasicHeader(description)}>
            {children}
        </Card>
    );
};
